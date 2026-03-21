!function (t) {
  t.fn.dateAndTime = function () {
    t(this).each(function (e, a) {
      let r = t(a);
      if (void 0 === r.attr("class") || 0 === r.attr("class").length) return console.error("dateAndTime : You must have an attribute class to use this !"), !1;
      let n = r.attr("class"), l = r.val().split(" ")[0], s = r.val().split(" ")[1] || "00:00", u = n, x = r.attr("required") ? "required":"";
      if ("text" !== r.attr("type")) return console.error("js_datetime : You must have an attribute text to use this !"), !1;
      u.length > 0 && r.removeClass(u);
      let i = t(`
			<input type="date" value="${l}" class="${u}" ${x}>
			<input type="time" value="${s}" class="${u}" ${x}>
			`);
      r.before(i).attr("done", !0).hide(), t(i, t(r).not('[done="true"]')).change(function (e) {
        let a = t(e.currentTarget);
        return a.attr("value", a.val()), "date" === a.attr("type") ? (l = a.val(), s = "00:00") : s = a.val(), r.attr("value", l + " " + s), !1
      })
    })
  }
}(jQuery);