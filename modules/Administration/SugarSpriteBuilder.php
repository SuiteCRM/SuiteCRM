<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */


require_once("include/SugarTheme/cssmin.php");

#[\AllowDynamicProperties]
class SugarSpriteBuilder
{
    public $isAvailable = false;
    public $silentRun = false;
    public $fromSilentUpgrade = false;
    public $writeToUpgradeLog = false;

    public $debug = false;
    public $fileName = 'sprites';
    public $cssMinify = true;

    // class supported image types
    public $supportedTypeMap = array(
        IMG_GIF => IMAGETYPE_GIF,
        IMG_JPG => IMAGETYPE_JPEG,
        IMG_PNG => IMAGETYPE_PNG,
    );

    // sprite settings
    public $pngCompression = 9;
    public $pngFilter = PNG_NO_FILTER;
    public $maxWidth = 75;
    public $maxHeight = 75;
    public $rowCnt = 30;

    // processed image types
    public $imageTypes = array();

    // source files
    public $spriteSrc = array();
    public $spriteRepeat = array();

    // sprite resource images
    public $spriteImg;

    // sprite_config collection
    public $sprites_config = array();


    public function __construct()
    {
        // check if we have gd installed
        if (function_exists('imagecreatetruecolor')) {
            $this->isAvailable = true;
            foreach ($this->supportedTypeMap as $gd_bit => $imagetype) {
                if (imagetypes() & $gd_bit) {
                    // swap gd_bit & imagetype
                    $this->imageTypes[$imagetype] = $gd_bit;
                }
            }
        }

        if (function_exists('logThis') && isset($GLOBALS['path'])) {
            $this->writeToUpgradeLog = true;
        }
    }


    /**
     * addDirectory
     *
     * This function is used to create the spriteSrc array
     * @param $name String value of the sprite name
     * @param $dir String value of the directory associated with the sprite entry
     */
    public function addDirectory($name, $dir)
    {

        // sprite namespace
        if (!array_key_exists($name, $this->spriteSrc)) {
            $this->spriteSrc[$name] = array();
        }

        // add files from directory
        $this->spriteSrc[$name][$dir] = $this->getFileList($dir);
    }

    /**
     * getFileList
     *
     * This method processes files in a directory and adds them to the sprites array
     * @param $dir String value of the directory to scan for image files in
     */
    private function getFileList($dir)
    {
        $list = array();
        if (is_dir($dir)) {
            if ($dh = opendir($dir)) {

                // optional sprites_config.php file
                $this->loadSpritesConfig($dir);

                while (($file = readdir($dh)) !== false) {
                    if ($file != "." && $file != ".." && $file != "sprites_config.php") {

                        // file info & check supported image format
                        if ($info = $this->getFileInfo($dir, $file)) {

                            // skip excluded files
                            if (isset($this->sprites_config[$dir]['exclude']) && array_search($file, $this->sprites_config[$dir]['exclude'], true) !== false) {
                                global $mod_strings;
                                $msg = string_format($mod_strings['LBL_SPRITES_EXCLUDING_FILE'], array("{$dir}/{$file}"));
                                $GLOBALS['log']->debug($msg);
                                $this->logMessage($msg);
                            } else {
                                // repeatable sprite ?
                                $isRepeat = false;

                                if (isset($this->sprites_config[$dir]['repeat'])) {
                                    foreach ($this->sprites_config[$dir]['repeat'] as $repeat) {
                                        if ($info['x'] == $repeat['width'] && $info['y'] == $repeat['height']) {
                                            $id = md5($repeat['width'].$repeat['height'].$repeat['direction']);
                                            $isRepeat = true;
                                            $this->spriteRepeat['repeat_'.$repeat['direction'].'_'.$id][$dir][$file] = $info;
                                        }
                                    }
                                }

                                if (!$isRepeat) {
                                    $list[$file] = $info;
                                }
                            }
                        } else {
                            if (preg_match('/\.(jpg|jpeg|gif|png|bmp|ico)$/i', $file)) {
                                $GLOBALS['log']->error('Unable to process image file ' . $file);
                                //$this->logMessage('Unable to process image file ' . $file);
                            }
                        }
                    }
                }
            }
            closedir($dh);
        }
        return $list;
    }


    /**
     * loadSpritesConfig
     *
     * This function is used to load the sprites_config.php file.  The sprites_config.php file may be used to add entries
     * to the sprites_config member variable which may contain a list of array entries of files/directories to exclude from
     * being included into the sprites image.
     *
     * @param $dir String value of the directory containing the custom sprites_config.php file
     */
    private function loadSpritesConfig($dir)
    {
        $sprites_config = array();
        if (file_exists("$dir/sprites_config.php")) {
            include("$dir/sprites_config.php");
            if (count($sprites_config)) {
                $this->sprites_config = array_merge($this->sprites_config, $sprites_config);
            }
        }
    }


    /**
     * getFileInfo
     *
     * This is a private helper function to return attributes about an image.  If the width, height or type of the
     * image file cannot be determined, then we do not process the file.
     *
     * @return array of file info entries containing file information (x, y, type) if image type is supported
     */
    private function getFileInfo($dir, $file)
    {
        $result = false;
        $info = @getimagesize($dir.'/'.$file);
        if ($info) {

            // supported image type ?
            if (isset($this->imageTypes[$info[2]])) {
                $w = $info[0];
                $h = $info[1];
                $surface = $w * $h;

                // be sure we have an image size
                $addSprite = false;
                if ($surface) {
                    // sprite dimensions
                    if ($w <= $this->maxWidth && $h <= $this->maxHeight) {
                        $addSprite = true;
                    }
                }

                if ($addSprite) {
                    $result = array();
                    $result['x'] = $w;
                    $result['y'] = $h;
                    $result['type'] = $info[2];
                }
            } else {
                $msg = "Skipping unsupported image file type ({$info[2]}) for file {$file}";
                $GLOBALS['log']->error($msg);
                $this->logMessage($msg."\n");
            }
        }
        return $result;
    }


    /**
     * createSprites
     *
     * This is the public function to allow the sprites to be built.
     *
     * @return $result boolean value indicating whether or not sprites were created
     */
    public function createSprites()
    {
        global $mod_strings;

        if (!$this->isAvailable) {
            if (!$this->silentRun) {
                $msg = $mod_strings['LBL_SPRITES_NOT_SUPPORTED'];
                $GLOBALS['log']->warn($msg);
                $this->logMessage($msg);
            }
            return false;
        }

        // add repeatable sprites
        if (count($this->spriteRepeat)) {
            $this->spriteSrc = array_merge($this->spriteSrc, $this->spriteRepeat);
        }

        foreach ($this->spriteSrc as $name => $dirs) {
            if (!$this->silentRun) {
                $msg = string_format($mod_strings['LBL_SPRITES_CREATING_NAMESPACE'], array($name));
                $GLOBALS['log']->debug($msg);
                $this->logMessage($msg);
            }

            // setup config for sprite placement algorithm
            if (substr((string) $name, 0, 6) == 'repeat') {
                $isRepeat = true;
                $type = substr((string) $name, 7, 10) == 'horizontal' ? 'horizontal' : 'vertical';
                $config = array(
                    'type' => $type,
                );
            } else {
                $isRepeat = false;
                $config = array(
                    'type' => 'boxed',
                    'width' => $this->maxWidth,
                    'height' => $this->maxHeight,
                    'rowcnt' => $this->rowCnt,
                );
            }

            // use separate class to arrange the images
            $sp = new SpritePlacement($dirs, $config);
            $sp->processSprites();

            //if(! $this->silentRun)
            //	echo " (size {$sp->width()}x{$sp->height()})<br />";

            // we need a target image size
            if ($sp->width() && $sp->height()) {
                // init sprite image
                $this->initSpriteImg($sp->width(), $sp->height());

                // add sprites based upon determined coordinates
                foreach ($dirs as $dir => $files) {
                    if (!$this->silentRun) {
                        $msg = string_format($mod_strings['LBL_SPRITES_PROCESSING_DIR'], array($dir));
                        $GLOBALS['log']->debug($msg);
                        $this->logMessage($msg);
                    }

                    foreach ($files as $file => $info) {
                        if ($im = $this->loadImage($dir, $file, $info['type'])) {
                            // coordinates
                            $dst_x = $sp->spriteMatrix[$dir.'/'.$file]['x'];
                            $dst_y = $sp->spriteMatrix[$dir.'/'.$file]['y'];

                            imagecopy($this->spriteImg, $im, $dst_x, $dst_y, 0, 0, $info['x'], $info['y']);
                            imagedestroy($im);

                            if (!$this->silentRun) {
                                $msg = string_format($mod_strings['LBL_SPRITES_ADDED'], array("{$dir}/{$file}"));
                                $GLOBALS['log']->debug($msg);
                                $this->logMessage($msg);
                            }
                        }
                    }
                }

                // dir & filenames
                if ($isRepeat) {
                    $outputDir = sugar_cached("sprites/Repeatable");
                    $spriteFileName = "{$name}.png";
                    $cssFileName = "{$this->fileName}.css";
                    $metaFileName = "{$this->fileName}.meta.php";
                    $nameSpace = "Repeatable";
                } else {
                    $outputDir = sugar_cached("sprites/$name");
                    $spriteFileName = "{$this->fileName}.png";
                    $cssFileName = "{$this->fileName}.css";
                    $metaFileName = "{$this->fileName}.meta.php";
                    $nameSpace = (string)($name);
                }

                // directory structure
                if (!is_dir(sugar_cached("sprites/$nameSpace"))) {
                    sugar_mkdir(sugar_cached("sprites/$nameSpace"), 0775, true);
                }

                // save sprite image
                imagepng($this->spriteImg, "$outputDir/$spriteFileName", $this->pngCompression, $this->pngFilter);
                imagedestroy($this->spriteImg);

                /* generate css & metadata */

                $head = '';
                $body = '';
                $metadata = '';

                foreach ($sp->spriteSrc as $id => $info) {
                    // sprite id
                    $hash_id = md5($id);

                    // header
                    $head .= "span.spr_{$hash_id},\n";

                    // image size
                    $w = $info['x'];
                    $h = $info['y'];

                    // image offset
                    $offset_x = $sp->spriteMatrix[$id]['x'];
                    $offset_y = $sp->spriteMatrix[$id]['y'];

                    // sprite css
                    $body .= "/* {$id} */
span.spr_{$hash_id} {
width: {$w}px;
height: {$h}px;
background-position: -{$offset_x}px -{$offset_y}px;
}\n";

                    $metadata .= '$sprites["'.$id.'"] = array ("class"=>"'.$hash_id.'","width"=>"'.$w.'","height"=>"'.$h.'");'."\n";
                }

                // common css header
                require_once('include/utils.php');
                $bg_path = getVersionedPath('index.php').'&entryPoint=getImage&imageName='.$spriteFileName.'&spriteNamespace='.$nameSpace;
                $head = rtrim($head, "\n,")." {background: url('../../../{$bg_path}'); no-repeat;display:inline-block;}\n";

                // append mode for repeatable sprites
                $fileMode = $isRepeat ? 'a' : 'w';

                // save css
                $css_content = "\n/* autogenerated sprites - $name */\n".$head.$body;
                if ($this->cssMinify) {
                    $css_content = cssmin::minify($css_content);
                }
                sugar_file_put_contents(
                    "$outputDir/$cssFileName",
                    $css_content,
                    $fileMode == 'a' ? FILE_APPEND : 0
                );

                /* save metadata */
                $add_php_tag = (file_exists("$outputDir/$metaFileName") && $isRepeat) ? false : true;
                $fh = sugar_fopen("$outputDir/$metaFileName", $fileMode);
                if ($add_php_tag) {
                    fwrite($fh, '<?php');
                }
                fwrite($fh, "\n/* sprites metadata - $name */\n");
                fwrite($fh, $metadata."\n");
                sugar_fclose($fh);

            // if width & height
            } else {
                if (!$this->silentRun) {
                    $msg = string_format($mod_strings['LBL_SPRITES_ADDED'], array($name));
                    $GLOBALS['log']->debug($msg);
                    $this->logMessage($msg);
                }
            }
        }
        return true;
    }


    /**
     * initSpriteImg
     *
     * @param w int value representing width of sprite
     * @param h int value representing height of sprite
     * Private function to initialize creating the sprite canvas image
     */
    private function initSpriteImg($w, $h)
    {
        $this->spriteImg = imagecreatetruecolor($w, $h);
        $transparent = imagecolorallocatealpha($this->spriteImg, 0, 0, 0, 127);
        imagefill($this->spriteImg, 0, 0, $transparent);
        imagealphablending($this->spriteImg, false);
        imagesavealpha($this->spriteImg, true);
    }


    /**
     * loadImage
     *
     * private function to load image resources
     *
     * @param $dir String value of directory where image is located
     * @param $file String value of file
     * @param $type String value of the file type (IMAGETYPE_GIF, IMAGETYPE_JPEG, IMAGETYPE_PNG)
     *
     */
    private function loadImage($dir, $file, $type)
    {
        $path_file = $dir.'/'.$file;
        switch ($type) {
            case IMAGETYPE_GIF:
                return imagecreatefromgif($path_file);
            case IMAGETYPE_JPEG:
                return imagecreatefromjpeg($path_file);
            case IMAGETYPE_PNG:
                return imagecreatefrompng($path_file);
            default:
                return false;
        }
    }

    /**
     * private logMessage
     *
     * This is a private function used to log messages generated from this class.  Depending on whether or not
     * silentRun or fromSilentUpgrade is set to true/false then it will either output to screen or write to log file
     *
     * @param $msg String value of message to log into file or echo into output buffer depending on the context
     */
    private function logMessage($msg)
    {
        if (!$this->silentRun && !$this->fromSilentUpgrade) {
            echo $msg . '<br />';
        } else {
            if ($this->fromSilentUpgrade && $this->writeToUpgradeLog) {
                logThis($msg, $GLOBALS['path']);
            } else {
                if (!$this->silentRun) {
                    echo $msg . "\n";
                }
            }
        }
    }
}


/**
 * SpritePlacement
 *
 */
#[\AllowDynamicProperties]
class SpritePlacement
{

    // occupied space
    public $spriteMatrix = array();

    // minimum surface
    public $minSurface = 0;

    // sprite src (flattened array)
    public $spriteSrc = array();

    // placement config array
    /*
    	type = 	boxed
    			horizontal
    			vertical

    	required params for
    	type 1 	-> width
    			-> height
    			-> rowcnt

    */
    public $config = array();

    public function __construct($spriteSrc, $config)
    {

        // convert spriteSrc to flat array
        foreach ($spriteSrc as $dir => $files) {
            foreach ($files as $file => $info) {
                // use full path as identifier
                $full_path = $dir.'/'.$file;
                $this->spriteSrc[$full_path] = $info;
            }
        }

        $this->config = $config;
    }

    public function processSprites()
    {
        foreach ($this->spriteSrc as $id => $info) {

            // dimensions
            $x = $info['x'];
            $y = $info['y'];

            // update min surface
            $this->minSurface += $x * $y;

            // get coordinates where to add this sprite
            if ($coor = $this->addSprite($x, $y)) {
                $this->spriteMatrix[$id] = $coor;
            }
        }
    }

    // returns x/y coordinates to fit the sprite
    public function addSprite($w, $h)
    {
        $result = false;

        switch ($this->config['type']) {

            // boxed
            case 'boxed':

                $spriteX = $this->config['width'];
                $spriteY = $this->config['height'];
                $spriteCnt = count($this->spriteMatrix) + 1;
                $y = ceil($spriteCnt / $this->config['rowcnt']);
                $x = $spriteCnt - (($y - 1) * $this->config['rowcnt']);
                $result = array(
                    'x' => ($x * $spriteX) + 1 - $spriteX,
                    'y' => ($y * $spriteY) + 1 - $spriteY);

                break;

            // horizontal -> align vertically
            case 'horizontal':
                $result = array('x' => 1, 'y' => $this->height() + 1);
                break;

            // vertical -> align horizontally
            case 'vertical':
                $result = array('x' => $this->width() + 1, 'y' => 1);
                break;

            default:
                $GLOBALS['log']->warn(self::class.": Unknown sprite placement algorithm -> {$this->config['type']}");
                break;
        }

        return $result;
    }

    // calculate total width
    public function width()
    {
        return $this->getMaxAxis('x');
    }

    // calculate total height
    public function height()
    {
        return $this->getMaxAxis('y');
    }

    // helper function to get highest axis value
    public function getMaxAxis($axis)
    {
        $val = 0;
        foreach ($this->spriteMatrix as $id => $coor) {
            $new_val = $coor[$axis] + $this->spriteSrc[$id][$axis] - 1;
            if ($new_val > $val) {
                $val = $new_val;
            }
        }
        return $val;
    }
}
