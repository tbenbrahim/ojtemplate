var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 42</i>';
states['fold000001'] = false;
texts['fold000044'] = '<a href="javascript:fold(\'fold000044\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 44 to line 47</i>';
states['fold000044'] = false;
texts['fold000049'] = '<a href="javascript:fold(\'fold000049\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 49 to line 59</i>';
states['fold000049'] = false;
texts['fold000061'] = '<a href="javascript:fold(\'fold000061\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 61 to line 62</i>';
states['fold000061'] = false;
texts['fold000066'] = '<a href="javascript:fold(\'fold000066\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 66 to line 68</i>';
states['fold000066'] = false;
texts['fold000070'] = '<a href="javascript:fold(\'fold000070\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 70 to line 90</i>';
states['fold000070'] = false;
texts['fold000092'] = '<a href="javascript:fold(\'fold000092\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 92 to line 94</i>';
states['fold000092'] = false;
texts['fold000096'] = '<a href="javascript:fold(\'fold000096\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 96 to line 97</i>';
states['fold000096'] = false;

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
