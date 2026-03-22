import network from './network.js';

async function fetchCommand(command, kwargs = {}) {
    const protocol = window.location.protocol;
    const host = window.location.host;
    const url = `${protocol}//${host}/api/commands/${command}`;

    const headers = { 'Content-Type': 'application/json' };
    const sessionId = network.getSessionId();
    if (sessionId) {
        headers['client-session'] = sessionId;
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
        depth: depth.toString(),
        before: before,
        "date-format": "date"
    });
}

export default { fetchCommand, log };