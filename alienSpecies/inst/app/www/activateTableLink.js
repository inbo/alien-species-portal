
function activateTableLink(o) {
	var id = o.getAttribute("data-id");
	var inputid = o.getAttribute("data-inputid");
	var d = new Date();
	Shiny.onInputChange(inputid, id + "_" + d.getHours() + d.getMinutes() + d.getSeconds());
}