<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


#[\AllowDynamicProperties]
class AOS_Product_CategoriesViewEdit extends ViewEdit
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        parent::display(); ?>
        <script>
            function update_parent_display(){
                var name_elem = document.getElementById("parent_category_name");
                var id_elem = document.getElementById("parent_category_id");
                var btn_elem = document.getElementById("btn_parent_category_name");
                var disable = document.getElementById("is_parent") && document.getElementById("is_parent").checked;
                if(name_elem){
                    name_elem.disabled = disable;
                }
                if(id_elem){
                    id_elem.disabled = disable;
                }
                if(btn_elem){
                    btn_elem.disabled = disable;
                }
                if(disable){
                    SUGAR.clearRelateField(this.form, 'parent_category_name', 'parent_category_id');
                }
            }
            document.getElementById("is_parent").onchange = update_parent_display;
            document.getElementById("is_parent").onchange();
        </script>

        <?php
    }
}
?>
