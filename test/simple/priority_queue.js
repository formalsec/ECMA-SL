
var PriorityQueue = (function () {

  var counter = 0;

  var Node = function (pri, val) {
    this.pri = pri;
    this.val = val;
    this.next = null;
    counter++
  }

  Node.prototype.insert = function (nl) {
    if (nl === null) {
      return this
    }

    if (this.pri > nl.pri) {
      this.next = nl;
      return this
    }

    var tmp = this.insert (nl.next);
    nl.next = tmp;
    return nl
  }

  var PQ = function () {
    this._head = null;
  };

  PQ.prototype.enqueue = function(pri, val) {
    var n = new Node(pri, val);
    this._head = n.insert(this._head);
  };

  PQ.prototype.dequeue = function () {
    if (this._head === null) {
      throw new Error("Queue is empty");
    }

    counter--;
    var first = this._head;
    this._head = this._head.next;
    return {pri: first.pri, val: first.val};
  };

   return PQ;
})();

var q, r;

q = new PriorityQueue();

q.enqueue (3, "foo");
q.enqueue (2, "baz");
q.enqueue (1, "bar");
r = (q.dequeue()).val;
AssertEquals(r, "foo");
