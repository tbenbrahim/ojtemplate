var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 629</i>';
states['fold000001'] = false;
texts['fold000631'] = '<a href="javascript:fold(\'fold000631\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 631 to line 660</i>';
states['fold000631'] = false;
texts['fold000662'] = '<a href="javascript:fold(\'fold000662\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 662 to line 873</i>';
states['fold000662'] = false;
texts['fold000876'] = '<a href="javascript:fold(\'fold000876\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 876 to line 880</i>';
states['fold000876'] = false;
texts['fold000882'] = '<a href="javascript:fold(\'fold000882\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 882 to line 1001</i>';
states['fold000882'] = false;
texts['fold001005'] = '<a href="javascript:fold(\'fold001005\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1005 to line 1282</i>';
states['fold001005'] = false;
texts['fold001285'] = '<a href="javascript:fold(\'fold001285\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1285 to line 1422</i>';
states['fold001285'] = false;
texts['fold001426'] = '<a href="javascript:fold(\'fold001426\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1426 to line 1430</i>';
states['fold001426'] = false;
texts['fold001432'] = '<a href="javascript:fold(\'fold001432\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1432 to line 1436</i>';
states['fold001432'] = false;
texts['fold001439'] = '<a href="javascript:fold(\'fold001439\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1439 to line 1451</i>';
states['fold001439'] = false;
texts['fold001456'] = '<a href="javascript:fold(\'fold001456\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1456 to line 1481</i>';
states['fold001456'] = false;
texts['fold001485'] = '<a href="javascript:fold(\'fold001485\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1485 to line 1511</i>';
states['fold001485'] = false;
texts['fold001514'] = '<a href="javascript:fold(\'fold001514\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1514 to line 1543</i>';
states['fold001514'] = false;

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
