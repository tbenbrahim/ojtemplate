var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 523</i>';
states['fold000001'] = false;
texts['fold000525'] = '<a href="javascript:fold(\'fold000525\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 525 to line 609</i>';
states['fold000525'] = false;
texts['fold000611'] = '<a href="javascript:fold(\'fold000611\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 611 to line 650</i>';
states['fold000611'] = false;
texts['fold000652'] = '<a href="javascript:fold(\'fold000652\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 652 to line 676</i>';
states['fold000652'] = false;
texts['fold000678'] = '<a href="javascript:fold(\'fold000678\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 678 to line 680</i>';
states['fold000678'] = false;

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
