var data_config = {
    tag: "visualization",
    mode: "local_files",

    title: "Overview of Openaire projects",
    input_format: "csv",
    base_unit: "readers",
    use_area_uri: false,
    is_force_areas: false,
    url_prefix: "",

    show_timeline: false,
    show_dropdown: true,
    show_intro: false,
    show_list:true,
    is_force_papers:true,

	show_context: false,
	create_title_from_context: false,

    files: [{
        title: "KNOW_AGINFRA",
        file: "./openaire/KNOW_AGINFRA.csv"
    }, {
        title: "KNOW_ALL-TIMES",
        file: "./openaire/KNOW_ALL-TIMES.csv"
    }, {
        title: "KNOW_CODE",
        file: "./openaire/KNOW_CODE.csv"
    }, {
        title: "KNOW_EEXCESS",
        file: "./openaire/KNOW_EEXCESS.csv"
    }, {
        title: "KNOW_GRASSMARGINS",
        file: "./openaire/KNOW_GRASSMARGINS.csv"
    }, {
        title: "KNOW_MIRROR",
        file: "./openaire/KNOW_MIRROR.csv"
    }, {
        title: "KNOW_WESENSEIT",
        file: "./openaire/KNOW_WESENSEIT.csv"
    }, {
        title: "KNOW_STELLAR",
        file: "./openaire/KNOW_STELLAR.csv"
    }, {
        title: "KNOW_WIQ-EI",
        file: "./openaire/KNOW_WIQ-EI.csv"
    }, {
        title: "KNOW_SEMAGROW",
        file: "./openaire/KNOW_SEMAGROW.csv"
    }]
};
