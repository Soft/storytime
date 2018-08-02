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
             title: false,
             author: false,
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
                     this.meta = json;
                     if ("title" in json) {
                         this.title = json.title;
                     }
                     if ("author" in json) {
                         this.author = json.author;
                     }
                 });
         },
         addCurrentSegment() {
             return fetch(api_current(this.session))
                 .then(response => response.json())
                 .then(json => {
                     this.segments.push(json);
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
                 .then(this.addCurrentSegment);
         }
     },
     created () {
         this.openSession()
             .then(this.fetchMeta)
             .then(this.addCurrentSegment);
     }
 };
</script>

<template>
    <main class="story">
        <header v-if="title || author">
            <h1 v-if="title">{{ title }}</h1>
            <span v-if="author" class="author">{{ author }}</span>
        </header>
        <segment v-for="(segment, index) in segments"
                 v-bind:key="index"
                 v-bind:active="index == segments.length - 1"
                 v-bind:text="segment.text"
                 v-bind:links="segment.links"
                 v-on:select="handleSelect" />
    </main>
</template>

<style scoped>
</style>
