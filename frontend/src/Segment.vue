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
                v-bind:key="index"
                v-bind:class="{ invalid: !link.valid }">
                <template v-if="active">
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
