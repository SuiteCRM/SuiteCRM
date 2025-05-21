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

class GoogleCloudAiplatformV1BatchPredictionJobInstanceConfig extends \Google\Collection
{
  protected $collection_key = 'includedFields';
  /**
   * @var string[]
   */
  public $excludedFields;
  /**
   * @var string[]
   */
  public $includedFields;
  /**
   * @var string
   */
  public $instanceType;
  /**
   * @var string
   */
  public $keyField;

  /**
   * @param string[]
   */
  public function setExcludedFields($excludedFields)
  {
    $this->excludedFields = $excludedFields;
  }
  /**
   * @return string[]
   */
  public function getExcludedFields()
  {
    return $this->excludedFields;
  }
  /**
   * @param string[]
   */
  public function setIncludedFields($includedFields)
  {
    $this->includedFields = $includedFields;
  }
  /**
   * @return string[]
   */
  public function getIncludedFields()
  {
    return $this->includedFields;
  }
  /**
   * @param string
   */
  public function setInstanceType($instanceType)
  {
    $this->instanceType = $instanceType;
  }
  /**
   * @return string
   */
  public function getInstanceType()
  {
    return $this->instanceType;
  }
  /**
   * @param string
   */
  public function setKeyField($keyField)
  {
    $this->keyField = $keyField;
  }
  /**
   * @return string
   */
  public function getKeyField()
  {
    return $this->keyField;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1BatchPredictionJobInstanceConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1BatchPredictionJobInstanceConfig');
