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

class GoogleCloudAiplatformV1ExplanationMetadataOutputMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $displayNameMappingKey;
  /**
   * @var array
   */
  public $indexDisplayNameMapping;
  /**
   * @var string
   */
  public $outputTensorName;

  /**
   * @param string
   */
  public function setDisplayNameMappingKey($displayNameMappingKey)
  {
    $this->displayNameMappingKey = $displayNameMappingKey;
  }
  /**
   * @return string
   */
  public function getDisplayNameMappingKey()
  {
    return $this->displayNameMappingKey;
  }
  /**
   * @param array
   */
  public function setIndexDisplayNameMapping($indexDisplayNameMapping)
  {
    $this->indexDisplayNameMapping = $indexDisplayNameMapping;
  }
  /**
   * @return array
   */
  public function getIndexDisplayNameMapping()
  {
    return $this->indexDisplayNameMapping;
  }
  /**
   * @param string
   */
  public function setOutputTensorName($outputTensorName)
  {
    $this->outputTensorName = $outputTensorName;
  }
  /**
   * @return string
   */
  public function getOutputTensorName()
  {
    return $this->outputTensorName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ExplanationMetadataOutputMetadata::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ExplanationMetadataOutputMetadata');
