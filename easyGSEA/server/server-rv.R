   rv <- reactiveValues(
        demo_mode = "", # "gsea" for a demo session for GSEA mode, "ora" for ORA mode, 
        demo_n = 1, # odd for load, even for unload
        demo_save = "no", # yes for saving the variables, no for regular run
        
        ora_genome_background = NULL, #NULL for initialization
        ora_option = "genome", selected_species_m="",
           
        run_mode=NULL, #gsea glist
        edge_mode=NULL, # edges of visnetwork: gs=gene set, lg=leadingedge
        lg_name = list("By similarities between leading-edge genes"="lg"
                       ,"By similarities between original gene sets"="gs"
                       ),
        run_n = 0,
        
        #rv for dendrogram
        dendro_run = NULL,
        cluster_bar_run = NULL,
        cutoff_point = 0.25,
        label_size = 4,
        cluster_size = 2, # minimum cluster size that has label
        max_cluster_size = 1,
        dendro_or_barplot = "dendro",
        abbreviate_check = FALSE,
        sort_check = FALSE,
        sort_pq = "cluster_id",
        color_check = "pval",
        abbreviate_length = 45,
        
        db_status=NULL, # selected
        db_modal=NULL, # check if modal if used or not
        gmt_cs=list(), gmt_cs_paths=list(), # uploaded GMTs for custom analysis
        gmt_cs_new=list(), gmt_cs_paths_new=list(), # newly uploaded GMTs temporarily stored
        gmt_temp=NULL, # temporary vector for input$gmt_c
        gmt_name_in_file=F, # if user-uploaded GMT has abbreviation already

        box_h_a = "594px", box_h = "574px", box_hp = 574, es_h = "245px",
        dendro_hp = 660,
        
        run_btn_color="warning", run_btn_style="simple",
        dbtn_style="unite", dbtn_color="default", dbtn_size="xs",
        nav_btn_color="warning", nav_btn_style="fill", nav_btn_size="md",
        
        #loaded GMT info
        GMTDF = data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("name", "size", "type", "datapath")))),
        
        #========================================#
        #####      RVs for GSEA run          #####
        #========================================#

        file_upload_status=NULL, # uploaded reset
        
        data_head=NULL, # to temporily store input data
        infile_confirm=NULL, # confirm, to see if user confirms file content to input
        infile_check=NULL, # wrong_rnk, wrong_deg, unmatch, pass
        
        rnk_or_deg=NULL,
        
        rnk_check = NULL, #none, low, pass
        
        total_genes=NULL,total_genes_after=NULL,
        
        # sampleRNK_yes=NULL,sampleDE_yes=NULL,#sampleRNKfile=NULL,sampleDEfile=NULL,
        example_file=NULL,
        
        
        infile_name=NULL,infile_path=NULL,
        rnkll=NULL,rnkgg=NULL,#deggg=NULL,
        
        tables_switch="up",
        
        #========================================#
        #####       RVs for ORA run          #####
        #========================================#
        glist_check = NULL, #none, low, pass
        
        sd_high = NULL,#rnkgg_scaled=NULL,
        
        data_glist=NULL,
        gene_lists=NULL, # vector that stores gene IDs input by user
        gene_lists_after=NULL, #converted gene IDs
        gene_lists_mat1=NULL, # df for id conversion (GSEA)
        gene_lists_mat2=NULL, # df for id conversion (ORA)
        
        ora_color="red", # color for bar/bubble/word/vis plot in ORA
        down_color="blue", up_color="red", # colors for bar/bubble/word/vis plot in GSEA

        # input_symbol=NULL, # check if input is SYMBOL, if, "yes"
        
        #========================================#
        ##### RVs for shared GSEA & ORA runs #####
        #========================================#
        q_dynamic=TRUE,plot_type="bar",
        db_name_y=T,db_id_y=T,

        # org_db=NULL,
        dbs=NULL,fgseagg=NULL,gmts=NULL,gmts_length=NULL,
        gmin=15,gmax=200,gperm=1000,
        no_up_01=0,no_down_01=0,no_up_05=0,no_down_05=0,
        
        bar_mode="cutoff", # or "gs" for selected Gene Sets
        bar_error=NULL, # "l" if data frame to large max 200 rows; 0 if 0 row
        bar_q_cutoff=1, volcano_cutoff=.005, # bar cutoff synchronized with bubble; bubble_q_cutoff=1, 
        bar_p_cutoff=.005, #bar cutoff synchronized with bubble; bubble_p_cutoff=.005,
        bar_up=10, bubble_up=10,
        bar_down=10, bubble_down=10,
        bar_pq="pval", volcano_pq="pval", #bar pq synchronized with bubble; bubble_pq="padj", 
        bar_pathway=NULL, bubble_pathway=NULL, volcano_pathway=NULL,
        bar_pathway_list=NULL,bubble_pathway_list=NULL,volcano_pathway_list=NULL,manhattan_pathway_list=NULL,
        bar_abb="y",bubble_abb="y",bar_abb_n=40,bubble_abb_n=40,
        
        bubble_zmin=2.5,bubble_zmax=9.5,
        
        volcano_mode="plotly", # plotly, plotly2, ggplot
        # volcano_top_down=5,volcano_top_up=5,
        volcano_name=NULL,
        
        p_volcano=NULL,
        
        n_word = 10, # no of words with top frequency
        
        k=1.5,
        
        es_term=NULL,rr=NULL,es_term_n=0,
        
        kegg_yes=NULL,kegg_confirm=NULL, #"yes"
        kegg_status=NULL,kegg_status_g=NULL,kegg_file_png=NULL,kegg_file_pdf=NULL,kegg_pos="topright", #cel04144.pathview.pdf
        
        reactome_yes=NULL,reactome_confirm=NULL,reactome_id=NULL,reactome_genes=NULL,#reactome_genes=paste0("['",paste(c("MED15","FYN"),collapse = "','"),"']"),
        wp_yes = NULL,wp_confirm=NULL,wp_id=NULL,wp_src=NULL,
        
        run=NULL, #success
        
        v=vector("list"), # dynamic UIs to display databases
        
        # hc_edges = NULL,
        vis=NULL,
        vis_status=NULL, #success if plotted, failed if df nrow==0
        vis_k=0.5,vis_p=.005,vis_q=1,vis_pq="pval",
        percent_method="jaccard",# or   combined overlap
        percent_cutoff = 0.25, # or 0.5 for overlap
        
        tl_p=1,tl_q=1,tl_ES="both",
        
        bar_tl=NULL,word_tl=NULL # tables for bar/bubble/keyword plots for download
    )