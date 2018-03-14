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
        title: "AGINFRA",
        file: "./openaire/AGINFRA.csv"
    }, {
        title: "GRASSMARGINS",
        file: "./openaire/GRASSMARGINS.csv"
    }, {
        title: "MIRROR",
        file: "./openaire/MIRROR.csv"
    }]
};
