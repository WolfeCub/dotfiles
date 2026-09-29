import definePlugin, { PluginNative } from "@utils/types";
import { findByPropsLazy } from "@webpack";

const Native = VencordNative.pluginHelpers.GlobalMute as PluginNative<typeof import("./native")>;
const AudioActions = findByPropsLazy("toggleSelfMute", "toggleSelfDeaf");

export default definePlugin({
    name: "GlobalMute",
    description: "Toggle mute from outside Vesktop by running vesktop-mute",
    authors: [{ name: "Josh Wolfe", id: 0n }],

    async start() {
        await Native.startWatching();
    },

    stop() {
        Native.stopWatching();
    },

    toggleMute() {
        AudioActions.toggleSelfMute();
    },
});
