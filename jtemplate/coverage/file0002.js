var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 38</i>';
states['fold000001'] = false;
texts['fold000040'] = '<a href="javascript:fold(\'fold000040\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 40 to line 41</i>';
states['fold000040'] = false;
texts['fold000043'] = '<a href="javascript:fold(\'fold000043\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 43 to line 45</i>';
states['fold000043'] = false;
texts['fold000050'] = '<a href="javascript:fold(\'fold000050\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 50 to line 64</i>';
states['fold000050'] = false;
texts['fold000066'] = '<a href="javascript:fold(\'fold000066\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 66 to line 274</i>';
states['fold000066'] = false;

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
