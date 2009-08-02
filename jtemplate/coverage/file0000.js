var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 524</i>';
states['fold000001'] = false;
texts['fold000526'] = '<a href="javascript:fold(\'fold000526\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 526 to line 610</i>';
states['fold000526'] = false;
texts['fold000612'] = '<a href="javascript:fold(\'fold000612\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 612 to line 651</i>';
states['fold000612'] = false;
texts['fold000653'] = '<a href="javascript:fold(\'fold000653\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 653 to line 677</i>';
states['fold000653'] = false;
texts['fold000679'] = '<a href="javascript:fold(\'fold000679\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 679 to line 681</i>';
states['fold000679'] = false;

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
