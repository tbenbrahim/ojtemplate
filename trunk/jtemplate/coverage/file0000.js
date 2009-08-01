var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 507</i>';
states['fold000001'] = false;
texts['fold000509'] = '<a href="javascript:fold(\'fold000509\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 509 to line 560</i>';
states['fold000509'] = false;
texts['fold000562'] = '<a href="javascript:fold(\'fold000562\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 562 to line 579</i>';
states['fold000562'] = false;
texts['fold000581'] = '<a href="javascript:fold(\'fold000581\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 581 to line 593</i>';
states['fold000581'] = false;
texts['fold000595'] = '<a href="javascript:fold(\'fold000595\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 595 to line 634</i>';
states['fold000595'] = false;
texts['fold000636'] = '<a href="javascript:fold(\'fold000636\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 636 to line 660</i>';
states['fold000636'] = false;
texts['fold000662'] = '<a href="javascript:fold(\'fold000662\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 662 to line 664</i>';
states['fold000662'] = false;

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
