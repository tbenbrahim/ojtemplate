var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 47</i>';
states['fold000001'] = false;
texts['fold000049'] = '<a href="javascript:fold(\'fold000049\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 49 to line 49</i>';
states['fold000049'] = false;
texts['fold000051'] = '<a href="javascript:fold(\'fold000051\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 51 to line 53</i>';
states['fold000051'] = false;
texts['fold000055'] = '<a href="javascript:fold(\'fold000055\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 55 to line 56</i>';
states['fold000055'] = false;
texts['fold000060'] = '<a href="javascript:fold(\'fold000060\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 60 to line 69</i>';
states['fold000060'] = false;
texts['fold000071'] = '<a href="javascript:fold(\'fold000071\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 71 to line 285</i>';
states['fold000071'] = false;

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
