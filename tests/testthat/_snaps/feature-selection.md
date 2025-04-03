# the feature select object is created correctly

    Code
      fs_obj
    Message
      == Feature Selection Object ====================================================
      -- Dataset Info ----------------------------------------------------------------
    Output
      * Rows                      100
      * Columns                   55
      * FeatureData               40
    Message
      -- Search Optimization Info ----------------------------------------------------
    Output
      * No. Candidates            '40'
      * Response Field            'class_response'
      * Cross Validation Runs     '5'
      * Cross Validation Folds    '5'
      * Stratified Folds          'FALSE'
      * Model Type                'fs_lr'
      * Search Type               'fs_forward_model'
      * Cost Function             'AUC'
      * Random Seed               '101'
      * Display Name              'Feature Selection Algorithm'
      * Search Complete           'FALSE'
    Message
      ================================================================================

# the feature select update() method works correctly

    Code
      fs_strat
    Message
      == Feature Selection Object ====================================================
      -- Dataset Info ----------------------------------------------------------------
    Output
      * Rows                      100
      * Columns                   55
      * FeatureData               40
    Message
      -- Search Optimization Info ----------------------------------------------------
    Output
      * No. Candidates            '40'
      * Response Field            'class_response'
      * Cross Validation Runs     '5'
      * Cross Validation Folds    '5'
      * Stratified Folds          'TRUE'
      * Model Type                'fs_lr'
      * Search Type               'fs_forward_model'
      * Cost Function             'AUC'
      * Random Seed               '101'
      * Display Name              'Feature Selection Algorithm'
      * Search Complete           'FALSE'
    Message
      ================================================================================

