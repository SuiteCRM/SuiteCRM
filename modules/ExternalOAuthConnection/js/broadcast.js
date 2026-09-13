const channel = new BroadcastChannel("auth");
channel.addEventListener ("message", (event) => {
    window.externalOAuthConnectionFields.setValue(event.data[0], event.data[1]);
});