<script>
 module.exports = {
     props: {
         active: Boolean,
         text: String,
         links: Array
     }
 };
</script>

<template>
    <section class="segment">
        <section>{{ text }}</section>
        <ol>
            <li v-for="(link, index) in links"
                v-bind:class="{ invalid: !link.valid }"
                v-on:key="index"
                v-on:click="$emit('select', index)">
                {{ link.text }}
            </li>
        </ol>
    </section>
</template>

<style scoped>
 .segment {
     font-family: sans;
 }
</style>
