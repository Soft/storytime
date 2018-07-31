<script>
 import 'whatwg-fetch';
 import Segment from "./Segment.vue";

 const api_register = "/api/register";
 const api_meta = "/api/meta";
 const api_current = session => `/api/${session}/current`;
 const api_select = session => `/api/${session}/select`;

 module.exports = {
     data: function() {
         return {
             session: false,
             meta: {},
             segments: []
         };
     },
     components: { Segment },
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
                     this.segments.push(json)
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
         },
     },
     created () {
         this.openSession()
             .then(this.fetchMeta)
             .then(this.fetchCurrent);
     }
 };
</script>

<template>
    <section class="story">
        <segment v-for="(segment, index) in segments"
                 v-bind:key="index"
                 v-bind:active="index == segments.length - 1"
                 v-bind:text="segment.text"
                 v-bind:links="segment.links"
                 v-on:select="handleSelect" />
    </section>
</template>

<style scoped>
</style>
