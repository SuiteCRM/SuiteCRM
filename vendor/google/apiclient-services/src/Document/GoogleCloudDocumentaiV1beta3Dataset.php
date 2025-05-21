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

class GoogleCloudDocumentaiV1beta3Dataset extends \Google\Model
{
  protected $documentWarehouseConfigType = GoogleCloudDocumentaiV1beta3DatasetDocumentWarehouseConfig::class;
  protected $documentWarehouseConfigDataType = '';
  protected $gcsManagedConfigType = GoogleCloudDocumentaiV1beta3DatasetGCSManagedConfig::class;
  protected $gcsManagedConfigDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  protected $spannerIndexingConfigType = GoogleCloudDocumentaiV1beta3DatasetSpannerIndexingConfig::class;
  protected $spannerIndexingConfigDataType = '';
  /**
   * @var string
   */
  public $state;
  protected $unmanagedDatasetConfigType = GoogleCloudDocumentaiV1beta3DatasetUnmanagedDatasetConfig::class;
  protected $unmanagedDatasetConfigDataType = '';

  /**
   * @param GoogleCloudDocumentaiV1beta3DatasetDocumentWarehouseConfig
   */
  public function setDocumentWarehouseConfig(GoogleCloudDocumentaiV1beta3DatasetDocumentWarehouseConfig $documentWarehouseConfig)
  {
    $this->documentWarehouseConfig = $documentWarehouseConfig;
  }
  /**
   * @return GoogleCloudDocumentaiV1beta3DatasetDocumentWarehouseConfig
   */
  public function getDocumentWarehouseConfig()
  {
    return $this->documentWarehouseConfig;
  }
  /**
   * @param GoogleCloudDocumentaiV1beta3DatasetGCSManagedConfig
   */
  public function setGcsManagedConfig(GoogleCloudDocumentaiV1beta3DatasetGCSManagedConfig $gcsManagedConfig)
  {
    $this->gcsManagedConfig = $gcsManagedConfig;
  }
  /**
   * @return GoogleCloudDocumentaiV1beta3DatasetGCSManagedConfig
   */
  public function getGcsManagedConfig()
  {
    return $this->gcsManagedConfig;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
  }
  /**
   * @param GoogleCloudDocumentaiV1beta3DatasetSpannerIndexingConfig
   */
  public function setSpannerIndexingConfig(GoogleCloudDocumentaiV1beta3DatasetSpannerIndexingConfig $spannerIndexingConfig)
  {
    $this->spannerIndexingConfig = $spannerIndexingConfig;
  }
  /**
   * @return GoogleCloudDocumentaiV1beta3DatasetSpannerIndexingConfig
   */
  public function getSpannerIndexingConfig()
  {
    return $this->spannerIndexingConfig;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param GoogleCloudDocumentaiV1beta3DatasetUnmanagedDatasetConfig
   */
  public function setUnmanagedDatasetConfig(GoogleCloudDocumentaiV1beta3DatasetUnmanagedDatasetConfig $unmanagedDatasetConfig)
  {
    $this->unmanagedDatasetConfig = $unmanagedDatasetConfig;
  }
  /**
   * @return GoogleCloudDocumentaiV1beta3DatasetUnmanagedDatasetConfig
   */
  public function getUnmanagedDatasetConfig()
  {
    return $this->unmanagedDatasetConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDocumentaiV1beta3Dataset::class, 'Google_Service_Document_GoogleCloudDocumentaiV1beta3Dataset');
