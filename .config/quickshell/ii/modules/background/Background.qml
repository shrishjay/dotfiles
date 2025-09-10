pragma ComponentBehavior: Bound

import qs
import qs.services
import qs.modules.common
import qs.modules.common.widgets
import qs.modules.common.functions as CF
import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.Hyprland

Variants {
    id: root
    readonly property bool fixedClockPosition: Config.options.background.fixedClockPosition
    readonly property real fixedClockX: Config.options.background.clockX
    readonly property real fixedClockY: Config.options.background.clockY
    model: Quickshell.screens

    PanelWindow {
        id: bgRoot

        required property var modelData

        readonly property Toplevel activeWindow: ToplevelManager.activeToplevel
        property bool focusingThisMonitor: HyprlandData.activeWorkspace?.monitor == monitor.name
        visible: !(activeWindow?.fullscreen && activeWindow?.activated && focusingThisMonitor)

        property HyprlandMonitor monitor: Hyprland.monitorFor(modelData)
        property list<var> relevantWindows: HyprlandData.windowList.filter(win => win.monitor == monitor.id && win.workspace.id >= 0).sort((a, b) => a.workspace.id - b.workspace.id)
        property int firstWorkspaceId: relevantWindows[0]?.workspace.id || 1
        property int lastWorkspaceId: relevantWindows[relevantWindows.length - 1]?.workspace.id || 10

        property bool wallpaperIsVideo: Config.options.background.wallpaperPath.endsWith(".mp4")
        || Config.options.background.wallpaperPath.endsWith(".webm")
        || Config.options.background.wallpaperPath.endsWith(".mkv")
        || Config.options.background.wallpaperPath.endsWith(".avi")
        || Config.options.background.wallpaperPath.endsWith(".mov")
        property string wallpaperPath: wallpaperIsVideo ? Config.options.background.thumbnailPath : Config.options.background.wallpaperPath
        property real preferredWallpaperScale: 1 // force no zoom
        property real effectiveWallpaperScale: 1 // force no zoom
        property int wallpaperWidth: modelData.width
        property int wallpaperHeight: modelData.height
        property real movableXSpace: 0 // no sliding
        property real movableYSpace: 0 // no sliding

        screen: modelData
        exclusionMode: ExclusionMode.Ignore
        WlrLayershell.layer: GlobalStates.screenLocked ? WlrLayer.Top : WlrLayer.Bottom
        WlrLayershell.namespace: "quickshell:background"
        anchors {
            top: true
            bottom: true
            left: true
            right: true
        }
        color: "transparent"

        onWallpaperPathChanged: {
            bgRoot.updateZoomScale()
        }

        function updateZoomScale() {
            getWallpaperSizeProc.path = bgRoot.wallpaperPath
            getWallpaperSizeProc.running = true;
        }
        Process {
            id: getWallpaperSizeProc
            property string path: bgRoot.wallpaperPath
            command: [ "magick", "identify", "-format", "%w %h", path ]
            stdout: StdioCollector {
                id: wallpaperSizeOutputCollector
                onStreamFinished: {
                    const output = wallpaperSizeOutputCollector.text
                    const [width, height] = output.split(" ").map(Number);
                    bgRoot.wallpaperWidth = width
                    bgRoot.wallpaperHeight = height

                    // Force no scaling
                    bgRoot.effectiveWallpaperScale = 1

                    bgRoot.updateClockPosition()
                }
            }
        }

        function updateClockPosition() {
            leastBusyRegionProc.path = bgRoot.wallpaperPath
            leastBusyRegionProc.contentWidth = 0
            leastBusyRegionProc.contentHeight = 0
            leastBusyRegionProc.horizontalPadding = 0
            leastBusyRegionProc.verticalPadding = 0
            leastBusyRegionProc.running = false;
            leastBusyRegionProc.running = true;
        }
        Process {
            id: leastBusyRegionProc
            property string path: bgRoot.wallpaperPath
            property int contentWidth: 0
            property int contentHeight: 0
            property int horizontalPadding: 0
            property int verticalPadding: 0
            command: ["true"]
        }
        Image {
            id: wallpaper
            visible: !bgRoot.wallpaperIsVideo
            source: bgRoot.wallpaperPath

            anchors.fill: parent
            fillMode: Image.PreserveAspectCrop // fill screen without distortion
            smooth: true
            asynchronous: false
            cache: true
        }


    }
}
