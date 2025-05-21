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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1ExplanationMetadataInputMetadata extends \Google\Collection
{
  protected $collection_key = 'inputBaselines';
  /**
   * @var string
   */
  public $denseShapeTensorName;
  /**
   * @var array[]
   */
  public $encodedBaselines;
  /**
   * @var string
   */
  public $encodedTensorName;
  /**
   * @var string
   */
  public $encoding;
  protected $featureValueDomainType = GoogleCloudAiplatformV1ExplanationMetadataInputMetadataFeatureValueDomain::class;
  protected $featureValueDomainDataType = '';
  /**
   * @var string
   */
  public $groupName;
  /**
   * @var string[]
   */
  public $indexFeatureMapping;
  /**
   * @var string
   */
  public $indicesTensorName;
  /**
   * @var array[]
   */
  public $inputBaselines;
  /**
   * @var string
   */
  public $inputTensorName;
  /**
   * @var string
   */
  public $modality;
  protected $visualizationType = GoogleCloudAiplatformV1ExplanationMetadataInputMetadataVisualization::class;
  protected $visualizationDataType = '';

  /**
   * @param string
   */
  public function setDenseShapeTensorName($denseShapeTensorName)
  {
    $this->denseShapeTensorName = $denseShapeTensorName;
  }
  /**
   * @return string
   */
  public function getDenseShapeTensorName()
  {
    return $this->denseShapeTensorName;
  }
  /**
   * @param array[]
   */
  public function setEncodedBaselines($encodedBaselines)
  {
    $this->encodedBaselines = $encodedBaselines;
  }
  /**
   * @return array[]
   */
  public function getEncodedBaselines()
  {
    return $this->encodedBaselines;
  }
  /**
   * @param string
   */
  public function setEncodedTensorName($encodedTensorName)
  {
    $this->encodedTensorName = $encodedTensorName;
  }
  /**
   * @return string
   */
  public function getEncodedTensorName()
  {
    return $this->encodedTensorName;
  }
  /**
   * @param string
   */
  public function setEncoding($encoding)
  {
    $this->encoding = $encoding;
  }
  /**
   * @return string
   */
  public function getEncoding()
  {
    return $this->encoding;
  }
  /**
   * @param GoogleCloudAiplatformV1ExplanationMetadataInputMetadataFeatureValueDomain
   */
  public function setFeatureValueDomain(GoogleCloudAiplatformV1ExplanationMetadataInputMetadataFeatureValueDomain $featureValueDomain)
  {
    $this->featureValueDomain = $featureValueDomain;
  }
  /**
   * @return GoogleCloudAiplatformV1ExplanationMetadataInputMetadataFeatureValueDomain
   */
  public function getFeatureValueDomain()
  {
    return $this->featureValueDomain;
  }
  /**
   * @param string
   */
  public function setGroupName($groupName)
  {
    $this->groupName = $groupName;
  }
  /**
   * @return string
   */
  public function getGroupName()
  {
    return $this->groupName;
  }
  /**
   * @param string[]
   */
  public function setIndexFeatureMapping($indexFeatureMapping)
  {
    $this->indexFeatureMapping = $indexFeatureMapping;
  }
  /**
   * @return string[]
   */
  public function getIndexFeatureMapping()
  {
    return $this->indexFeatureMapping;
  }
  /**
   * @param string
   */
  public function setIndicesTensorName($indicesTensorName)
  {
    $this->indicesTensorName = $indicesTensorName;
  }
  /**
   * @return string
   */
  public function getIndicesTensorName()
  {
    return $this->indicesTensorName;
  }
  /**
   * @param array[]
   */
  public function setInputBaselines($inputBaselines)
  {
    $this->inputBaselines = $inputBaselines;
  }
  /**
   * @return array[]
   */
  public function getInputBaselines()
  {
    return $this->inputBaselines;
  }
  /**
   * @param string
   */
  public function setInputTensorName($inputTensorName)
  {
    $this->inputTensorName = $inputTensorName;
  }
  /**
   * @return string
   */
  public function getInputTensorName()
  {
    return $this->inputTensorName;
  }
  /**
   * @param string
   */
  public function setModality($modality)
  {
    $this->modality = $modality;
  }
  /**
   * @return string
   */
  public function getModality()
  {
    return $this->modality;
  }
  /**
   * @param GoogleCloudAiplatformV1ExplanationMetadataInputMetadataVisualization
   */
  public function setVisualization(GoogleCloudAiplatformV1ExplanationMetadataInputMetadataVisualization $visualization)
  {
    $this->visualization = $visualization;
  }
  /**
   * @return GoogleCloudAiplatformV1ExplanationMetadataInputMetadataVisualization
   */
  public function getVisualization()
  {
    return $this->visualization;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ExplanationMetadataInputMetadata::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ExplanationMetadataInputMetadata');
