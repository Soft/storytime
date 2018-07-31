import Vue from 'vue';
import "./main.css";
import Story from "./Story.vue";

const app = new Vue({
    el: "#root",
    render: h => h(Story)
});
