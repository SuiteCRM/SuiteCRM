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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1DataDiscoverySpecStorageConfig extends \Google\Collection
{
  protected $collection_key = 'includePatterns';
  protected $csvOptionsType = GoogleCloudDataplexV1DataDiscoverySpecStorageConfigCsvOptions::class;
  protected $csvOptionsDataType = '';
  /**
   * @var string[]
   */
  public $excludePatterns;
  /**
   * @var string[]
   */
  public $includePatterns;
  protected $jsonOptionsType = GoogleCloudDataplexV1DataDiscoverySpecStorageConfigJsonOptions::class;
  protected $jsonOptionsDataType = '';

  /**
   * @param GoogleCloudDataplexV1DataDiscoverySpecStorageConfigCsvOptions
   */
  public function setCsvOptions(GoogleCloudDataplexV1DataDiscoverySpecStorageConfigCsvOptions $csvOptions)
  {
    $this->csvOptions = $csvOptions;
  }
  /**
   * @return GoogleCloudDataplexV1DataDiscoverySpecStorageConfigCsvOptions
   */
  public function getCsvOptions()
  {
    return $this->csvOptions;
  }
  /**
   * @param string[]
   */
  public function setExcludePatterns($excludePatterns)
  {
    $this->excludePatterns = $excludePatterns;
  }
  /**
   * @return string[]
   */
  public function getExcludePatterns()
  {
    return $this->excludePatterns;
  }
  /**
   * @param string[]
   */
  public function setIncludePatterns($includePatterns)
  {
    $this->includePatterns = $includePatterns;
  }
  /**
   * @return string[]
   */
  public function getIncludePatterns()
  {
    return $this->includePatterns;
  }
  /**
   * @param GoogleCloudDataplexV1DataDiscoverySpecStorageConfigJsonOptions
   */
  public function setJsonOptions(GoogleCloudDataplexV1DataDiscoverySpecStorageConfigJsonOptions $jsonOptions)
  {
    $this->jsonOptions = $jsonOptions;
  }
  /**
   * @return GoogleCloudDataplexV1DataDiscoverySpecStorageConfigJsonOptions
   */
  public function getJsonOptions()
  {
    return $this->jsonOptions;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataDiscoverySpecStorageConfig::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataDiscoverySpecStorageConfig');
