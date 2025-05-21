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

namespace Google\Service\Contactcenterinsights;

class GoogleCloudContactcenterinsightsV1Dimension extends \Google\Model
{
  protected $agentDimensionMetadataType = GoogleCloudContactcenterinsightsV1DimensionAgentDimensionMetadata::class;
  protected $agentDimensionMetadataDataType = '';
  /**
   * @var string
   */
  public $dimensionKey;
  protected $issueDimensionMetadataType = GoogleCloudContactcenterinsightsV1DimensionIssueDimensionMetadata::class;
  protected $issueDimensionMetadataDataType = '';
  protected $qaQuestionAnswerDimensionMetadataType = GoogleCloudContactcenterinsightsV1DimensionQaQuestionAnswerDimensionMetadata::class;
  protected $qaQuestionAnswerDimensionMetadataDataType = '';
  protected $qaQuestionDimensionMetadataType = GoogleCloudContactcenterinsightsV1DimensionQaQuestionDimensionMetadata::class;
  protected $qaQuestionDimensionMetadataDataType = '';

  /**
   * @param GoogleCloudContactcenterinsightsV1DimensionAgentDimensionMetadata
   */
  public function setAgentDimensionMetadata(GoogleCloudContactcenterinsightsV1DimensionAgentDimensionMetadata $agentDimensionMetadata)
  {
    $this->agentDimensionMetadata = $agentDimensionMetadata;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1DimensionAgentDimensionMetadata
   */
  public function getAgentDimensionMetadata()
  {
    return $this->agentDimensionMetadata;
  }
  /**
   * @param string
   */
  public function setDimensionKey($dimensionKey)
  {
    $this->dimensionKey = $dimensionKey;
  }
  /**
   * @return string
   */
  public function getDimensionKey()
  {
    return $this->dimensionKey;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1DimensionIssueDimensionMetadata
   */
  public function setIssueDimensionMetadata(GoogleCloudContactcenterinsightsV1DimensionIssueDimensionMetadata $issueDimensionMetadata)
  {
    $this->issueDimensionMetadata = $issueDimensionMetadata;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1DimensionIssueDimensionMetadata
   */
  public function getIssueDimensionMetadata()
  {
    return $this->issueDimensionMetadata;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1DimensionQaQuestionAnswerDimensionMetadata
   */
  public function setQaQuestionAnswerDimensionMetadata(GoogleCloudContactcenterinsightsV1DimensionQaQuestionAnswerDimensionMetadata $qaQuestionAnswerDimensionMetadata)
  {
    $this->qaQuestionAnswerDimensionMetadata = $qaQuestionAnswerDimensionMetadata;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1DimensionQaQuestionAnswerDimensionMetadata
   */
  public function getQaQuestionAnswerDimensionMetadata()
  {
    return $this->qaQuestionAnswerDimensionMetadata;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1DimensionQaQuestionDimensionMetadata
   */
  public function setQaQuestionDimensionMetadata(GoogleCloudContactcenterinsightsV1DimensionQaQuestionDimensionMetadata $qaQuestionDimensionMetadata)
  {
    $this->qaQuestionDimensionMetadata = $qaQuestionDimensionMetadata;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1DimensionQaQuestionDimensionMetadata
   */
  public function getQaQuestionDimensionMetadata()
  {
    return $this->qaQuestionDimensionMetadata;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1Dimension::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1Dimension');
