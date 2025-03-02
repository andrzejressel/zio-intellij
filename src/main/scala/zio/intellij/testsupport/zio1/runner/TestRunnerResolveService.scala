package zio.intellij.testsupport.zio1.runner

import com.intellij.notification.Notification
import com.intellij.openapi.components.{PersistentStateComponent, State, Storage}
import com.intellij.openapi.progress.{ProcessCanceledException, ProgressIndicator}
import com.intellij.openapi.project.Project
import com.intellij.util.concurrency.AppExecutorUtil
import com.intellij.util.xmlb.XmlSerializerUtil
import org.jetbrains.annotations.{Nls, NonNls}
import org.jetbrains.plugins.scala.ScalaVersion
import zio.intellij.testsupport.ZTestRunConfiguration.ZTestRunnerName
import TestRunnerDownloader.DownloadResult.{DownloadFailure, DownloadSuccess}
import TestRunnerDownloader.{DownloadProgressListener, NoopProgressListener}
import TestRunnerResolveService.ResolveError.DownloadError
import TestRunnerResolveService._
import zio.intellij.utils.{BackgroundTask, ScalaVersionHack, ZioVersion}

import java.net.{URL, URLClassLoader}
import java.util.concurrent.ConcurrentHashMap
import scala.beans.BeanProperty
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala
import scala.ref.WeakReference
import scala.util._

// Borrowed from ScalafmtDynamicServiceImpl and friends

@State(name = "TestRunnerResolveService", storages = Array(new Storage("zio_testrunner_resolve_cache.xml")))
private[testsupport] final class TestRunnerResolveService
    extends PersistentStateComponent[TestRunnerResolveService.ServiceState] {

  private val testRunnerVersions: mutable.Map[(ZioVersion, String), ResolveStatus] =
    new ConcurrentHashMap[(ZioVersion, String), ResolveStatus]().asScala

  private val state: TestRunnerResolveService.ServiceState = new TestRunnerResolveService.ServiceState

  override def getState: TestRunnerResolveService.ServiceState = state
  override def loadState(state: TestRunnerResolveService.ServiceState): Unit =
    XmlSerializerUtil.copyBean(state, this.state)

  def resolve(
    version: ZioVersion,
    scalaVersion: ScalaVersion,
    downloadIfMissing: Boolean,
    resolveFast: Boolean = false,
    progressListener: DownloadProgressListener = NoopProgressListener
  ): ResolveResult = testRunnerVersions.get((version, scalaVersion.versionStr)) match {
    case Some(ResolveStatus.Resolved(jarPaths)) => Right(jarPaths)
    case _ if resolveFast                       => Left(ResolveError.NotFound(version, scalaVersion))
    case Some(ResolveStatus.DownloadInProgress) => Left(ResolveError.DownloadInProgress(version, scalaVersion))
    case _ =>
      val key = s"${version.toString}###${scalaVersion.versionStr}"
      if (state.resolvedVersions.containsKey(key)) {
        val jarUrls = state.resolvedVersions.get(key).map(new URL(_))
        resolveClassPath(version, scalaVersion, jarUrls.toIndexedSeq) match {
          case r @ Right(_)                 => r
          case Left(_) if downloadIfMissing => downloadAndResolve(version, scalaVersion, progressListener)
          case _                            => Left(ResolveError.NotFound(version, scalaVersion))
        }
      } else if (downloadIfMissing) downloadAndResolve(version, scalaVersion, progressListener)
      else Left(ResolveError.NotFound(version, scalaVersion))
  }

  private val appExecutorService                   = AppExecutorUtil.getAppExecutorService
  implicit val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(appExecutorService)

  def resolveAsync(
    version: ZioVersion,
    scalaVersion: ScalaVersion,
    project: Project
  ): Future[ResolveResult] =
    testRunnerVersions.get((version, scalaVersion.versionStr)) match {
      case Some(ResolveStatus.Resolved(fmt)) =>
        Future.successful(Right(fmt))
      case Some(ResolveStatus.DownloadInProgress) =>
        Future.successful(Left(ResolveError.DownloadInProgress(version, scalaVersion)))
      case _ =>
        @NonNls val title = s"Downloading the ZIO Test runner for ZIO $version"
        val task = BackgroundTask(project, title = title, cancelText = "Cancel downloading ZIO Test runner...") {
          indicator =>
            val progressListener = new ProgressIndicatorDownloadListener(indicator, title)
            resolve(version, scalaVersion, downloadIfMissing = true, progressListener = progressListener)
        }
        task.recover {
          case pce: ProcessCanceledException =>
            Left(DownloadError(version, scalaVersion, pce))
        }
    }

  private def downloadAndResolve(
    version: ZioVersion,
    scalaVersion: ScalaVersion,
    listener: DownloadProgressListener = NoopProgressListener
  ): ResolveResult = {
    val downloader = new TestRunnerDownloader(listener)
    downloader.download(version)(scalaVersion).left.map(ResolveError.DownloadError.apply).flatMap {
      case DownloadSuccess(v, scalaVersion, jarUrls) => resolveClassPath(v, scalaVersion, jarUrls)
    }
  }

  private def resolveClassPath(version: ZioVersion, scalaVersion: ScalaVersion, jarUrls: Seq[URL]): ResolveResult = {
    val urls: Array[URL] = jarUrls.toArray
    Try(new URLClassLoader(urls, null).loadClass(ZTestRunnerName + "$")) match {
      case Success(_) =>
        val key = s"${version.toString}###${scalaVersion.versionStr}"
        state.resolvedVersions.put(key, jarUrls.toArray.map(_.toString))
        testRunnerVersions((version, scalaVersion.versionStr)) = ResolveStatus.Resolved(jarUrls)
        Right(jarUrls)
      case Failure(e) =>
        Left(ResolveError.UnknownError(version, scalaVersion, e))
    }
  }

  private[runner] def clearCaches() = testRunnerVersions.clear()
}
object TestRunnerResolveService {
  def instance(project: Project): TestRunnerResolveService = project.getService(classOf[TestRunnerResolveService])

  type ResolveResult = Either[ResolveError, Seq[URL]]

  sealed trait ResolveStatus
  object ResolveStatus {
    object DownloadInProgress                     extends ResolveStatus
    final case class Resolved(jarPaths: Seq[URL]) extends ResolveStatus
  }

  sealed trait ResolveError
  object ResolveError {
    final case class NotFound(version: ZioVersion, scalaVersion: ScalaVersion)           extends ResolveError
    final case class DownloadInProgress(version: ZioVersion, scalaVersion: ScalaVersion) extends ResolveError
    final case class DownloadError(version: ZioVersion, scalaVersion: ScalaVersion, cause: Throwable)
        extends ResolveError
    final case class UnknownError(version: ZioVersion, scalaVersion: ScalaVersion, cause: Throwable)
        extends ResolveError

    object DownloadError {
      def apply(f: DownloadFailure): DownloadError = new DownloadError(f.version, f.scalaVersion, f.cause)
    }
  }

  final class ServiceState() {
    // ZIO version -> list of classpath jar URLs
    @BeanProperty
    var resolvedVersions: java.util.Map[String, Array[String]] = new java.util.TreeMap()
  }

  private class ProgressIndicatorDownloadListener(indicator: ProgressIndicator, @Nls prefix: String = "")
      extends DownloadProgressListener {
    override def progressUpdate(message: String): Unit = {
      if (message.nonEmpty) {
        //noinspection ReferencePassedToNls,ScalaExtractStringToBundle
        indicator.setText(prefix + ": " + message)
      }
      indicator.checkCanceled()
    }
    override def doProgress(): Unit =
      indicator.checkCanceled()
  }
}
