<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

namespace SuiteCRM\PDF\LegacyMPDF;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use mPDF;
use SuiteCRM\PDF\PDFEngine;

require_once __DIR__ . '/../../../modules/AOS_PDF_Templates/PDF_Lib/mpdf.php';

/**
 * Class LegacyMPDFEngine
 * @package SuiteCRM\PDF\LegacyMPDF
 */
#[\AllowDynamicProperties]
class LegacyMPDFEngine extends PDFEngine
{
    /**
     * @var mPDF
     */
    public $pdf;

    /**
     * @var string
     */
    private static $configMapperFile = __DIR__ . '/../../../lib/PDF/LegacyMPDF/configMapping.php';

    /**
     * MPDFEngine constructor.
     * @param mPDF|null $pdf
     */
    public function __construct(mPDF $pdf = null)
    {
        @$this->pdf = $pdf ?? new mPDF();
    }

    /**
     * @param string $html
     * @param int $section 0=default; 1=headerCSS; 2=HTML body; 3=HTML parses; 4=HTML headers;
     * @param bool $init Leaves buffers, etc, in current state so that it can continue a block.
     * @param bool $close Clears and sets buffers to top level block.
     * @return void
     */
    public function writeHTML(string $html, int $section = 0, bool $init = true, bool $close = true): void
    {
        @$this->pdf->WriteHTML($html, $section, $init, $close);

        if ($section === 1) {
            @$this->pdf->SetDefaultBodyCSS('background-color', '#FFFFFF');
            unset($this->pdf->cssmgr->CSS['INPUT']['FONT-SIZE']);
        }
    }

    /**
     * @param string $name
     * @param string $destination
     * @param string $fullName
     * @return void|string
     */
    public function outputPDF(string $name, string $destination, string $fullName = ''): ?string
    {
        @$output = $this->pdf->Output($name, $destination);

        if (is_string($output)) {
            return $output;
        }
    }

    /**
     * @param string|array $html
     * @return void
     */
    public function writeHeader(string $html): void
    {
        @$this->pdf->SetHTMLHeader($html);
    }

    /**
     * @param string|array $html
     * @return void
     */
    public function writeFooter(string $html): void
    {
        @$this->pdf->SetHTMLFooter($html);
    }

    /**
     * @param string $css
     * @return void
     */
    public function addCSS(string $css): void
    {
        $this->writeHTML($css, 1);
    }
    
    public function writeBlankPage(): void
    {
        @$this->pdf->AddPage();
    }

    /**
     * @param array $options
     * @return void
     */
    public function configurePDF(array $options): void
    {
        /** @noinspection PhpIncludeInspection */
        $configOptions = include self::$configMapperFile;

        @$this->pdf = new mPDF(
            $configOptions['mode'],
            $configOptions['page_size'].'-'.$configOptions['orientation'],
            $configOptions['default_font_size'],
            $configOptions['default_font'],
            $configOptions['margin_left'],
            $configOptions['margin_right'],
            $configOptions['margin_top'],
            $configOptions['margin_bottom'],
            $configOptions['margin_header'],
            $configOptions['margin_footer'],
            $configOptions['orientation'],
        );

        @$this->pdf->SetAutoFont();
    }
}
