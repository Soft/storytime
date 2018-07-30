import Vue from 'vue';
import 'whatwg-fetch';
import "./main.css";

const api_register = "/api/register";
const api_meta = "/api/meta";
const api_current = session => `/api/${session}/current`;
const api_select = session => `/api/${session}/select`;

const app = new Vue({
    el: "#root",
    data: {
        session: false,
        meta: {},
        text: "",
        links: []
    },
    methods: {
        openSession() {
            return fetch(api_register, { method: "POST" })
                .then(response => response.json())
                .then(json => this.session = json.sessionID);
        },
        fetchMeta() {
            return fetch(api_meta)
                .then(response => response.json())
                .then(json => {
                    this.meta = json.meta;
                });
        },
        fetchCurrent() {
            return fetch(api_current(this.session))
                .then(response => response.json())
                .then(json => {
                    this.text = json.text;
                    this.links = json.links;
                });
        },
        selectLink(index) {
            let data = new URLSearchParams();
            data.append("linkIndex", index.toString());
            return fetch(api_select(this.session),
                         { method: "POST", body: data });
        },
        handleSelect(index) {
            this.selectLink(index)
                .then(this.fetchCurrent);
        }
    },
    created () {
        this.openSession()
            .then(this.fetchMeta)
            .then(this.fetchCurrent);
    },
    render () {
        return (<section>
                  <section className="content">{ this.text }</section>
                  <ol className="link">
                  {this.links.map((link, index) =>
                    <li onClick={ this.handleSelect.bind(this, index) }>{ link }</li>)}
                  </ol>
                </section>);
    }
});

