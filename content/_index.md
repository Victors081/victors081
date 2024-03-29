---
date: "2022-10-24"
sections:
- block: hero
  content:
    cta:
      label: '**Get Started**'
      url: https://hugoblox.com/templates/
    cta_alt:
      label: Ask a question
      url: https://discord.gg/z8wNYzb
    cta_note:
      label: '<div style="text-shadow: none;"><a class="github-button" href="https://github.com/HugoBlox/hugo-blox-builder"
        data-icon="octicon-star" data-size="large" data-show-count="true" aria-label="Star">Star
        Hugo Blox Builder</a></div><div style="text-shadow: none;"><a class="github-button"
        href="https://github.com/HugoBlox/theme-academic-cv" data-icon="octicon-star"
        data-size="large" data-show-count="true" aria-label="Star">Star the Academic
        template</a></div>'
    image:
      filename: hero-academic.png
    text: |-
      **Generated by Hugo Blox Builder - the FREE, Hugo-based open source website builder trusted by 500,000+ sites.**

      **Easily build anything with blocks - no-code required!**

      From landing pages, second brains, and courses to academic resumés, conferences, and tech blogs.

      <!--Custom spacing-->
      <div class="mb-3"></div>
      <!--GitHub Button JS-->
      <script async defer src="https://buttons.github.io/buttons.js"></script>
    title: Hugo Academic Theme
  demo: true
  design:
    background:
      gradient_end: '#1976d2'
      gradient_start: '#004ba0'
      text_color_light: true

- block: about.biography
  content:
    title: Welcome
    username: admin
  id: about

- block: accomplishments
  content:
      # Note: `&shy;` is used to add a 'soft' hyphen in a long heading.
      title: 'Grants and Awards'
      subtitle:
      # Date format: https://docs.hugoblox.com/customization/#date-format
      date_format: Jan 2006
      # Accomplishments.
      #   Add/remove as many `item` blocks below as you like.
      #   `title`, `organization`, and `date_start` are the required parameters.
      #   Leave other parameters empty if not required.
      #   Begin multi-line descriptions with YAML's `|2-` multi-line prefix.
      items:
        - date_end: ''
          date_start: '2023-10-31'
          description: ''
          organization: 'DAAD Deutscher Akademischer Austauschdienst'
          title: Scholarship Studienreisen für Gruppen von ausländischen Studierenden in Deutschland
          url: ''
        - date_end: ''
          date_start: '2023-01-01'
          description: ''
          organization: 'Universidad Icesi'
          title: Master’s degree Scholarship
          url: ''
        - date_end: ''
          date_start: '2022-08-01'
          description: ''
          organization: 'Universidad Icesi'
          title: Scholarship tuition fees
          url: ''
        - date_end: ''
          date_start: '2022-07-01'
          description: ''
          organization: 'National Economics Academic Competition'
          title: Competitor
          url: ''
        - date_end: ''
          date_start: '2021-05-01'
          description: ''
          organization: 'Economic and Political Developments in China and Colombia'
          title: Icesi ‑ Hong Kong short‑term virtual exchange program
          url: ''
        - date_end: ''
          date_start: '2017-11-01'
          description: ''
          organization: 'Ser Pilo Paga 4.0'
          title: Undergraduate Scholarship
          url: ''
  design:
      columns: '2'


- block: markdown
  content:
    subtitle: ""
    text: '{{< gallery album="demo" >}}'
    title: Gallery
  design:
    columns: "1"

- block: collection
  content:
    filters:
      exclude_featured: true
      folders:
      - publication
      tag: ''
      category: ''
      publication_type: ''
      author: ''
      featured_only: false
      exclude_featured: false
      exclude_future: false
      exclude_past: true
      # Choose how many pages you would like to display (0 = all pages)
      count: 5
      # Choose how many pages you would like to offset by
      # Useful if you wish to show the first item in the Featured widget
      offset: 0
      # Field to sort by, such as Date or Title
      sort_by: 'Date'
      sort_ascending: false
    text: |-
      {{% callout note %}}
      Quickly discover relevant content by [filtering publications](./publication/).
      {{% /callout %}}

      Alonso, Julio C, Sarmiento, Victor M (2023). [Retorno de la ocupación a los niveles de la prepandemia](./publication/retorno-de-la-ocupaci-n-a-los-niveles-de-la-prepandemia/). in Actualización del sistema de cuentas económicas distritales de Santiago de Cali 2015-2022.
    title: Recent Publications
  design:
    columns: "2"
    view: citation
  id: featured
- block: collection
  content:
    filters:
      folders:
      - event
    title: Recent Talks
  design:
    columns: "2"
    view: compact
  id: talks
- block: contact
  content:
    email: vmsarmiento@icesi.edu.co
    form:
      formspree:
        id: null
      netlify:
        captcha: false
      provider: netlify
    subtitle: null
    text: null
    title: Contact
  design:
    columns: "2"
  id: contact
title: ""
type: landing
---
