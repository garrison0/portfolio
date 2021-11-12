<template>
  <div class="grid__description">
    <v-container>
      <v-row>
        <v-col class="col-12 col-sm-8"><v-toolbar-title class="text-truncate text-h5">{{title}}</v-toolbar-title>
        </v-col>
        <v-col class="col-12 col-sm-4 mb-1" 
            :class="{'text-end': $vuetify.breakpoint.smAndUp}">
          <v-btn target="_blank" :href="link" class="white red--text font-weight-bold">watch it &nbsp; <v-icon class="mt-1">mdi-youtube</v-icon></v-btn>
        </v-col>
      </v-row>
      <v-divider class="my-3"></v-divider>
      <v-row class="my-5 justify-center" :class="{ 'mx-5': $vuetify.breakpoint.smAndUp }">
        <v-col class="col-12"> 
          <v-row class="justify-center">
            <v-col class="col-2 col-sm-1">
            <v-icon>mdi-head-question</v-icon>
            </v-col>
            <v-col class="col-10 col-sm-9 mt-1">
              {{description}}
            </v-col>
          </v-row>
        </v-col>
      </v-row>
    </v-container>
  </div>
</template>


<script>
  export default {
    name: 'GridItem',

    props: ['title', 'description', 'link'],
  }

</script>