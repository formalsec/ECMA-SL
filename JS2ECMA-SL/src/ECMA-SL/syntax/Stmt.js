class Stmt {}

Stmt.fromJSON = function (obj) {
   switch (obj.type) {
       case "skip": return new Skip(); 
       case "print": return Print.fromJSON(obj); 
       case "assign": return Assign.fromJSON(obj); 
       default: throw new Error("Unsupported statement: "+obj.type); 
   }
}

module.exports = Stmt
