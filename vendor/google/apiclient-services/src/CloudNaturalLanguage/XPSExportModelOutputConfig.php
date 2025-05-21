<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\CloudNaturalLanguage;

class XPSExportModelOutputConfig extends \Google\Model
{
  protected $coreMlFormatType = XPSCoreMlFormat::class;
  protected $coreMlFormatDataType = '';
  protected $dockerFormatType = XPSDockerFormat::class;
  protected $dockerFormatDataType = '';
  protected $edgeTpuTfLiteFormatType = XPSEdgeTpuTfLiteFormat::class;
  protected $edgeTpuTfLiteFormatDataType = '';
  /**
   * @var bool
   */
  public $exportFirebaseAuxiliaryInfo;
  /**
   * @var string
   */
  public $outputGcrUri;
  /**
   * @var string
   */
  public $outputGcsUri;
  protected $tfJsFormatType = XPSTfJsFormat::class;
  protected $tfJsFormatDataType = '';
  protected $tfLiteFormatType = XPSTfLiteFormat::class;
  protected $tfLiteFormatDataType = '';
  protected $tfSavedModelFormatType = XPSTfSavedModelFormat::class;
  protected $tfSavedModelFormatDataType = '';

  /**
   * @param XPSCoreMlFormat
   */
  public function setCoreMlFormat(XPSCoreMlFormat $coreMlFormat)
  {
    $this->coreMlFormat = $coreMlFormat;
  }
  /**
   * @return XPSCoreMlFormat
   */
  public function getCoreMlFormat()
  {
    return $this->coreMlFormat;
  }
  /**
   * @param XPSDockerFormat
   */
  public function setDockerFormat(XPSDockerFormat $dockerFormat)
  {
    $this->dockerFormat = $dockerFormat;
  }
  /**
   * @return XPSDockerFormat
   */
  public function getDockerFormat()
  {
    return $this->dockerFormat;
  }
  /**
   * @param XPSEdgeTpuTfLiteFormat
   */
  public function setEdgeTpuTfLiteFormat(XPSEdgeTpuTfLiteFormat $edgeTpuTfLiteFormat)
  {
    $this->edgeTpuTfLiteFormat = $edgeTpuTfLiteFormat;
  }
  /**
   * @return XPSEdgeTpuTfLiteFormat
   */
  public function getEdgeTpuTfLiteFormat()
  {
    return $this->edgeTpuTfLiteFormat;
  }
  /**
   * @param bool
   */
  public function setExportFirebaseAuxiliaryInfo($exportFirebaseAuxiliaryInfo)
  {
    $this->exportFirebaseAuxiliaryInfo = $exportFirebaseAuxiliaryInfo;
  }
  /**
   * @return bool
   */
  public function getExportFirebaseAuxiliaryInfo()
  {
    return $this->exportFirebaseAuxiliaryInfo;
  }
  /**
   * @param string
   */
  public function setOutputGcrUri($outputGcrUri)
  {
    $this->outputGcrUri = $outputGcrUri;
  }
  /**
   * @return string
   */
  public function getOutputGcrUri()
  {
    return $this->outputGcrUri;
  }
  /**
   * @param string
   */
  public function setOutputGcsUri($outputGcsUri)
  {
    $this->outputGcsUri = $outputGcsUri;
  }
  /**
   * @return string
   */
  public function getOutputGcsUri()
  {
    return $this->outputGcsUri;
  }
  /**
   * @param XPSTfJsFormat
   */
  public function setTfJsFormat(XPSTfJsFormat $tfJsFormat)
  {
    $this->tfJsFormat = $tfJsFormat;
  }
  /**
   * @return XPSTfJsFormat
   */
  public function getTfJsFormat()
  {
    return $this->tfJsFormat;
  }
  /**
   * @param XPSTfLiteFormat
   */
  public function setTfLiteFormat(XPSTfLiteFormat $tfLiteFormat)
  {
    $this->tfLiteFormat = $tfLiteFormat;
  }
  /**
   * @return XPSTfLiteFormat
   */
  public function getTfLiteFormat()
  {
    return $this->tfLiteFormat;
  }
  /**
   * @param XPSTfSavedModelFormat
   */
  public function setTfSavedModelFormat(XPSTfSavedModelFormat $tfSavedModelFormat)
  {
    $this->tfSavedModelFormat = $tfSavedModelFormat;
  }
  /**
   * @return XPSTfSavedModelFormat
   */
  public function getTfSavedModelFormat()
  {
    return $this->tfSavedModelFormat;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSExportModelOutputConfig::class, 'Google_Service_CloudNaturalLanguage_XPSExportModelOutputConfig');
