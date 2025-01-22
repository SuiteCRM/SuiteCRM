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

namespace SuiteCRM\PDF\TCPDF;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use SuiteCRM\PDF\PDFEngine;

/**
 * Class TFPDFEngine
 * @package SuiteCRM\PDF\TCPDF
 */
#[\AllowDynamicProperties]
class TCPDFEngine extends PDFEngine
{
    /**
     * @var SuiteTCPDF
     */
    public $pdf;

    /**
     * @var array
     */
    public $pdfOptions;

    /**
     * @var string
     */
    protected $defaultCSS = '';
    
    /**
     * @var string
     */
    protected $css = '';

    /**
     * @var string
     */
    private static $configMapperFile = __DIR__ . '/../../../lib/PDF/TCPDF/configMapping.php';

    /**
     * TFPDFEngine constructor.
     * @param SuiteTCPDF|null $pdf
     */
    public function __construct(SuiteTCPDF $pdf = null)
    {
        $this->pdf = $pdf ?? new SuiteTCPDF();
        
        $stylesheet = file_get_contents(__DIR__ . '/../../../lib/PDF/TCPDF/default.css');
        $this->defaultCSS = $stylesheet;
    }

    /**
     * @param string $html
     * @return void
     */
    public function writeHTML(string $html): void
    {
        if ($this->pdf->getNumPages() < 1) {
            $this->writeBlankPage();
        }

        $this->pdf->writeHTML($html . $this->getCSS());
    }

    /**
     * @param string $name
     * @param string $destination
     * @param string $fullName
     * @return string|null
     */
    public function outputPDF(string $name, string $destination, string $fullName = ''): ?string
    {
        if ($destination === 'D') {
            $this->pdf->Output($name, $destination);
        } else {
            $this->pdf->Output(__DIR__ . '/../../../' . $name, $destination);
        }

        return null;
    }

    /**
     * @param string|array $html
     * @return void
     */
    public function writeHeader(string $html): void
    {
        $this->pdf->setPrintHeader(true);
        $this->pdf->setHtmlHeader($html);
    }

    /**
     * @param string|array $html
     * @return void
     */
    public function writeFooter(string $html): void
    {
        $this->pdf->setPrintFooter(true);
        $this->pdf->setHtmlFooter($html);
    }

    /**
     * @param string $css
     * @return void
     */
    public function addCSS(string $css): void
    {
        $this->css = $css;
    }

    /**
     * @return string
     */
    public function getCSS(): string 
    {
        return '<style>' . $this->css . $this->defaultCSS . '</style>';
    }

    public function writeBlankPage(): void
    {
        $this->pdf->AddPage();
    }

    /**
     * @param array $options
     * @return void
     */
    public function configurePDF(array $options): void
    {
        /** @noinspection PhpIncludeInspection */
        $configOptions = include self::$configMapperFile;
        $this->pdfOptions = $configOptions;

        $this->pdf = new SuiteTCPDF(
            $configOptions['orientation'],
            $configOptions['unit'],
            $configOptions['page_size'],
            true,
            'UTF-8',
            false
        );

        $this->pdf->SetMargins(
            $configOptions['margin_left'],
            $configOptions['margin_top'],
            $configOptions['margin_right']
        );

        $tagvs = ['div' => [['h' => 0, 'n' => 0], ['h' => 0, 'n' => 0]]];
        $this->pdf->setHtmlVSpace($tagvs);

        $this->pdf->setPrintHeader(false);
        $this->pdf->setPrintFooter(false);

        $this->pdf->setHeaderMargin($configOptions['margin_header']);
        $this->pdf->setFooterMargin($configOptions['margin_footer']);
        $this->pdf->SetAutoPageBreak(true, $configOptions['margin_bottom']);
        $this->pdf->setImageScale($configOptions['image_scale']);
        
        $this->pdf->SetFont($configOptions['default_font'], '', $configOptions['default_font_size']);
        $this->pdf->setHeaderFont([$configOptions['default_font'], '', $configOptions['default_font_size']]);
        $this->pdf->setFooterFont([$configOptions['default_font'], '', $configOptions['default_font_size']]);

    }
}
