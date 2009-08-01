var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 133</i>';
states['fold000001'] = false;
texts['fold000135'] = '<a href="javascript:fold(\'fold000135\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 135 to line 205</i>';
states['fold000135'] = false;
texts['fold000207'] = '<a href="javascript:fold(\'fold000207\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 207 to line 235</i>';
states['fold000207'] = false;
texts['fold000237'] = '<a href="javascript:fold(\'fold000237\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 237 to line 277</i>';
states['fold000237'] = false;
texts['fold000279'] = '<a href="javascript:fold(\'fold000279\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 279 to line 293</i>';
states['fold000279'] = false;
texts['fold000295'] = '<a href="javascript:fold(\'fold000295\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 295 to line 368</i>';
states['fold000295'] = false;
texts['fold000371'] = '<a href="javascript:fold(\'fold000371\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 371 to line 418</i>';
states['fold000371'] = false;
texts['fold000420'] = '<a href="javascript:fold(\'fold000420\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 420 to line 516</i>';
states['fold000420'] = false;
texts['fold000518'] = '<a href="javascript:fold(\'fold000518\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 518 to line 530</i>';
states['fold000518'] = false;
texts['fold000532'] = '<a href="javascript:fold(\'fold000532\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 532 to line 568</i>';
states['fold000532'] = false;
texts['fold000570'] = '<a href="javascript:fold(\'fold000570\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 570 to line 570</i>';
states['fold000570'] = false;

function fold(id) {
  tmp = document.all[id].innerHTML;
  document.all[id].innerHTML = texts[id];
  texts[id] = tmp;
  states[id] = !(states[id]);
}

function unfoldAll() {
  for (key in states) {
    if (states[key]) {
      fold(key);
    }
  }
}

function foldAll() {
  for (key in states) {
    if (!(states[key])) {
      fold(key);
    }
  }
}
