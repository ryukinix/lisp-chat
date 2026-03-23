import auth from './auth.js';

async function fetchCommand(command, kwargs = {}, provideSession = false) {
    const protocol = window.location.protocol;
    const host = window.location.host;
    const url = `${protocol}//${host}/api/commands/${command}`;

    const headers = {
        'Content-Type': 'application/json'
    };

    if (provideSession) {
        const sessionId = auth.getSessionId();
        if (!sessionId) {
            throw new Error(`Session ID is not defined. Cannot call API command: ${command}`);
        }
        headers['Client-Session'] = sessionId;
    }

    const response = await fetch(url, {
        method: 'POST',
        headers: headers,
        body: JSON.stringify({ kwargs })
    });

    if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
    }

    return await response.json();
}

async function log(depth, before) {
    return fetchCommand('log', {
        "depth": depth.toString(),
        "before": before,
        "date-format": "date"
    }, true);
}

async function channels(all = true) {
    return fetchCommand('channels', {
        "all": all
    });
}

export default { fetchCommand, log, channels };
