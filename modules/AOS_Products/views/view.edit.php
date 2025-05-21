<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


#[\AllowDynamicProperties]
class AOS_ProductsViewEdit extends ViewEdit
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        global $app_strings,$sugar_config;

        isset($this->bean->product_image) ? $image = $this->bean->product_image : $image = '';


        $temp = str_replace($sugar_config['site_url'].'/'.$sugar_config['upload_dir'], "", (string) $image);
        $html = '<span id=\'new_attachment\' style=\'display:';
        if (!empty($this->bean->product_image)) {
            $html .= 'none';
        }
        $html .= '\'><input name="uploadimage" tabindex="3" type="file" size="60"/>
        	</span>
		<span id=\'old_attachment\' style=\'display:';
        if (empty($image)) {
            $html .= 'none';
        }
        $html .= '\'><input type=\'hidden\' id=\'deleteAttachment\' name=\'deleteAttachment\' value=\'0\'>
		'.$temp.'<input type=\'hidden\' name=\'old_product_image\' value=\''.$image.'\'/>
		<input type=\'button\' class=\'button\' value=\''.$app_strings['LBL_REMOVE'].'\' onclick=\'deleteProductImage();\' >
		</span>';

        $this->ss->assign('PRODUCT_IMAGE', $html);
        parent::display();
    }
}
