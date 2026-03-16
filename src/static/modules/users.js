import utils from './utils.js';

function updateUserList(usersString) {
    const userList = document.getElementById("user-list");
    if (!userList) return;

    const users = usersString.replace("users: ", "").split(",").map(u => u.trim()).filter(u => u.length > 0);
    userList.innerHTML = "";
    users.forEach(user => {
        const li = document.createElement("li");
        li.className = "user-item";
        li.textContent = user;
        li.style.color = utils.getUserColor(user);
        userList.appendChild(li);
    });
}

export default { updateUserList };
