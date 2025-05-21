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

namespace Google\Service\Document;

class GoogleCloudDocumentaiV1OcrConfig extends \Google\Collection
{
  protected $collection_key = 'advancedOcrOptions';
  /**
   * @var string[]
   */
  public $advancedOcrOptions;
  /**
   * @var bool
   */
  public $computeStyleInfo;
  /**
   * @var bool
   */
  public $disableCharacterBoxesDetection;
  /**
   * @var bool
   */
  public $enableImageQualityScores;
  /**
   * @var bool
   */
  public $enableNativePdfParsing;
  /**
   * @var bool
   */
  public $enableSymbol;
  protected $hintsType = GoogleCloudDocumentaiV1OcrConfigHints::class;
  protected $hintsDataType = '';
  protected $premiumFeaturesType = GoogleCloudDocumentaiV1OcrConfigPremiumFeatures::class;
  protected $premiumFeaturesDataType = '';

  /**
   * @param string[]
   */
  public function setAdvancedOcrOptions($advancedOcrOptions)
  {
    $this->advancedOcrOptions = $advancedOcrOptions;
  }
  /**
   * @return string[]
   */
  public function getAdvancedOcrOptions()
  {
    return $this->advancedOcrOptions;
  }
  /**
   * @param bool
   */
  public function setComputeStyleInfo($computeStyleInfo)
  {
    $this->computeStyleInfo = $computeStyleInfo;
  }
  /**
   * @return bool
   */
  public function getComputeStyleInfo()
  {
    return $this->computeStyleInfo;
  }
  /**
   * @param bool
   */
  public function setDisableCharacterBoxesDetection($disableCharacterBoxesDetection)
  {
    $this->disableCharacterBoxesDetection = $disableCharacterBoxesDetection;
  }
  /**
   * @return bool
   */
  public function getDisableCharacterBoxesDetection()
  {
    return $this->disableCharacterBoxesDetection;
  }
  /**
   * @param bool
   */
  public function setEnableImageQualityScores($enableImageQualityScores)
  {
    $this->enableImageQualityScores = $enableImageQualityScores;
  }
  /**
   * @return bool
   */
  public function getEnableImageQualityScores()
  {
    return $this->enableImageQualityScores;
  }
  /**
   * @param bool
   */
  public function setEnableNativePdfParsing($enableNativePdfParsing)
  {
    $this->enableNativePdfParsing = $enableNativePdfParsing;
  }
  /**
   * @return bool
   */
  public function getEnableNativePdfParsing()
  {
    return $this->enableNativePdfParsing;
  }
  /**
   * @param bool
   */
  public function setEnableSymbol($enableSymbol)
  {
    $this->enableSymbol = $enableSymbol;
  }
  /**
   * @return bool
   */
  public function getEnableSymbol()
  {
    return $this->enableSymbol;
  }
  /**
   * @param GoogleCloudDocumentaiV1OcrConfigHints
   */
  public function setHints(GoogleCloudDocumentaiV1OcrConfigHints $hints)
  {
    $this->hints = $hints;
  }
  /**
   * @return GoogleCloudDocumentaiV1OcrConfigHints
   */
  public function getHints()
  {
    return $this->hints;
  }
  /**
   * @param GoogleCloudDocumentaiV1OcrConfigPremiumFeatures
   */
  public function setPremiumFeatures(GoogleCloudDocumentaiV1OcrConfigPremiumFeatures $premiumFeatures)
  {
    $this->premiumFeatures = $premiumFeatures;
  }
  /**
   * @return GoogleCloudDocumentaiV1OcrConfigPremiumFeatures
   */
  public function getPremiumFeatures()
  {
    return $this->premiumFeatures;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDocumentaiV1OcrConfig::class, 'Google_Service_Document_GoogleCloudDocumentaiV1OcrConfig');
