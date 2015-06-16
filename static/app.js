"use strict";

function urlEncode(obj) {
	return Object.keys(obj)
		.map(function(k) {
			return encodeURIComponent(k) + "=" + encodeURIComponent(obj[k]);
		}).join("&");
}

function request(url, attrs) {
	var success = attrs.success || function () {};
	var error = attrs.error || function () {};
	var method = attrs.method || "GET";
	var req = new XMLHttpRequest();
	req.responseType = "text";
	req.addEventListener("load", function () {
		if (req.readyState === 4) {
			if (req.status === 200) {
				success(req.responseText);
			} else {
				error(req.responseText, req.status);
			}
		}
	});
	req.addEventListener("error", function() {
		error("Connection error");
	});
	req.open(method, url, true);
	if (attrs.data) {
		req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
		req.send(urlEncode(attrs.data));
	} else {
		req.send();
	}
}

function Notifier(root) {
	this.tray = document.createElement("div");
	this.tray.setAttribute("class", "tray");
	root.appendChild(this.tray);
};

Notifier.prototype = {
	send: function(str) {
		var bubble = document.createElement("div");
		bubble.textContent = str;
		bubble.addEventListener("click", this.tray.removeChild.bind(this.tray, bubble));
		this.tray.appendChild(bubble);
	}
};

function applyMeta() {
	var actions = {
		"title": function(str) {
			document.title = str;
		}
	};
	request("/api/meta", {
		success: function(str) {
			var data = JSON.parse(str);
			Object.keys(data).forEach(function(k) {
				if (actions.hasOwnProperty(k)) {
					actions[k](data[k]);
				}
			});
		}
	});
}

(function() {
	var API_CURRENT = "/api/current";
	var API_SELECT = "/api/select";

	var clearElement = function(el) {
		while(el.firstChild) {
			el.removeChild(el.firstChild);
		}
	};

	var format = function(str) {
		var parSplit = /(\n|\r|\r\n){2,}/;
		var frag = document.createDocumentFragment();
		str.trim().split(parSplit).forEach(function(par) {
			var p = document.createElement("p");
			p.textContent = par;
			frag.appendChild(p);
		});
		return frag;
	};

	var handleKeyEvent = function(ev) {
		if (ev.key.match(/\d/)) {
			var num = parseInt(ev.key, 10);
			if (num > 0) {
				this.navigate(num - 1);
			}
		}
	};

	window.View = function View(root) {
		this.root = root;
		this.state = { text: "", links: [] };

		this.content = document.createElement("article");
		root.appendChild(this.content);

		var navContainer = document.createElement("nav");
		this.nav = document.createElement("ol");
		root.appendChild(navContainer);
		navContainer.appendChild(this.nav);

		this.notifier = new Notifier(document.body);

		document.addEventListener("keypress", handleKeyEvent.bind(this));
	};

	window.View.prototype = {
		refresh: function(callback) {
			request(API_CURRENT, {
				success: function(str) {
					this.state = JSON.parse(str);
					callback();
				}.bind(this)
			});
		},

		navigate: function(index) {
			request(API_SELECT, {
				method: "POST",
				data: { "id": index },
				success: this.redisplay.bind(this),
				error: function(str, status) {
					if (status === 500) {
						var data = JSON.parse(str);
						this.notifier.send(data.error);
					} else {
						this.notifier.send(str);
					}
				}.bind(this)
			});
			return false;
		},

		render: function() {
			clearElement(this.content);
			clearElement(this.nav);
			this.content.appendChild(format(this.state.text));
			this.state.links.forEach(function(str, ind) {
				var item = document.createElement("li");
				var link = document.createElement("a");
				link.addEventListener("click", this.navigate.bind(this, ind));
				link.setAttribute("href", "#");
				link.textContent = str;
				item.appendChild(link);
				this.nav.appendChild(item);
			}, this);
		},

		redisplay: function() {
			this.refresh(this.render.bind(this));
		}
	};

})();

document.addEventListener("DOMContentLoaded", function() {
	var view = new View(document.querySelector("#root"));
	view.redisplay();
	applyMeta();
});

