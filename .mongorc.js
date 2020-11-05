// Created: Lungang Fang 11/04/2020 
// Modified: Lungang Fang 11/05/2020 14:59>

DBQuery.prototype._prettyShell = true

createMyAccount = function() {
    db.getSiblingDB('admin').createUser(
        {user:'lgfang', pwd:'hello.world', roles: ["root"]})
}

login = function() {
    db.getSiblingDB('admin').auth('lgfang', 'hello.world')
    return db.runCommand({connectionStatus : 1}).authInfo
}
