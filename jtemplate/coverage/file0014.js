var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 45</i>';
states['fold000001'] = false;
texts['fold000047'] = '<a href="javascript:fold(\'fold000047\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 47 to line 48</i>';
states['fold000047'] = false;
texts['fold000050'] = '<a href="javascript:fold(\'fold000050\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 50 to line 51</i>';
states['fold000050'] = false;
texts['fold000053'] = '<a href="javascript:fold(\'fold000053\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 53 to line 55</i>';
states['fold000053'] = false;
texts['fold000057'] = '<a href="javascript:fold(\'fold000057\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 57 to line 62</i>';
states['fold000057'] = false;
texts['fold000065'] = '<a href="javascript:fold(\'fold000065\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 65 to line 68</i>';
states['fold000065'] = false;
texts['fold000070'] = '<a href="javascript:fold(\'fold000070\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 70 to line 70</i>';
states['fold000070'] = false;
texts['fold000073'] = '<a href="javascript:fold(\'fold000073\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 73 to line 76</i>';
states['fold000073'] = false;
texts['fold000078'] = '<a href="javascript:fold(\'fold000078\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 78 to line 95</i>';
states['fold000078'] = false;
texts['fold000097'] = '<a href="javascript:fold(\'fold000097\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 97 to line 97</i>';
states['fold000097'] = false;

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
