<script>
 module.exports = {
     props: {
         active: Boolean,
         text: String,
         links: Array
     },
     computed: {
         paragraphs: function() {
             let parSplit = /(\n|\r|\r\n){2,}/;
             return this.text.trim()
                        .split(parSplit)
                        .filter(p => p.trim().length != 0);
         }
     }
 };
</script>

<template>
    <section class="segment">
        <section>
            <p v-for="(p, index) in paragraphs"
               v-bind:key="index">
                {{ p }}
            </p>
        </section>
        <ol>
            <li v-for="(link, index) in links"
                v-bind:key="index"
                v-bind:class="{ invalid: !link.valid }">
                <template v-if="active && link.valid">
                    <a v-on:click.prevent="$emit('select', index)">{{ link.text }}</a>
                </template>
                <template v-else>
                    {{ link.text }}
                </template>
            </li>
        </ol>
    </section>
</template>

<style scoped>
 .segment {
     font-family: sans;
 }
</style>
