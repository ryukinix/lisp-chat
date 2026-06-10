import utils from './utils.js';

function getSuperscript(num) {
    const map = {
        '0': '⁰', '1': '¹', '2': '²', '3': '³', '4': '⁴',
        '5': '⁵', '6': '⁶', '7': '⁷', '8': '⁸', '9': '⁹'
    };
    return String(num).split('').map(char => map[char] || char).join('');
}

function updateUserList(usersString) {
    const userList = document.getElementById("user-list");
    if (!userList) return;

    const users = usersString.replace("users: ", "").split(",").map(u => u.trim()).filter(u => u.length > 0);
    userList.innerHTML = "";

    const userCounts = {};
    const order = [];
    users.forEach(user => {
        if (!userCounts[user]) {
            userCounts[user] = 0;
            order.push(user);
        }
        userCounts[user]++;
    });

    order.forEach(user => {
        const count = userCounts[user];
        const displayName = count > 1 ? `${user}${getSuperscript(count)}` : user;
        const li = document.createElement("li");
        li.className = "user-item";
        li.dataset.username = user;
        li.textContent = displayName;
        li.style.color = utils.getUserColor(user);
        userList.appendChild(li);
    });
}

export default { updateUserList };
