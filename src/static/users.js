import { getUserColor } from './utils.js';

export function updateUserList(usersString) {
    const userList = document.getElementById("user-list");
    if (!userList) return;

    const users = usersString.replace("users: ", "").split(",").map(u => u.trim()).filter(u => u.length > 0);
    userList.innerHTML = "";
    users.forEach(user => {
        const li = document.createElement("li");
        li.className = "user-item";
        li.textContent = user;
        li.style.color = getUserColor(user);
        userList.appendChild(li);
    });
}
