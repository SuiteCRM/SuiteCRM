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

class GoogleCloudAiplatformV1MigratableResource extends \Google\Model
{
  protected $automlDatasetType = GoogleCloudAiplatformV1MigratableResourceAutomlDataset::class;
  protected $automlDatasetDataType = '';
  protected $automlModelType = GoogleCloudAiplatformV1MigratableResourceAutomlModel::class;
  protected $automlModelDataType = '';
  protected $dataLabelingDatasetType = GoogleCloudAiplatformV1MigratableResourceDataLabelingDataset::class;
  protected $dataLabelingDatasetDataType = '';
  /**
   * @var string
   */
  public $lastMigrateTime;
  /**
   * @var string
   */
  public $lastUpdateTime;
  protected $mlEngineModelVersionType = GoogleCloudAiplatformV1MigratableResourceMlEngineModelVersion::class;
  protected $mlEngineModelVersionDataType = '';

  /**
   * @param GoogleCloudAiplatformV1MigratableResourceAutomlDataset
   */
  public function setAutomlDataset(GoogleCloudAiplatformV1MigratableResourceAutomlDataset $automlDataset)
  {
    $this->automlDataset = $automlDataset;
  }
  /**
   * @return GoogleCloudAiplatformV1MigratableResourceAutomlDataset
   */
  public function getAutomlDataset()
  {
    return $this->automlDataset;
  }
  /**
   * @param GoogleCloudAiplatformV1MigratableResourceAutomlModel
   */
  public function setAutomlModel(GoogleCloudAiplatformV1MigratableResourceAutomlModel $automlModel)
  {
    $this->automlModel = $automlModel;
  }
  /**
   * @return GoogleCloudAiplatformV1MigratableResourceAutomlModel
   */
  public function getAutomlModel()
  {
    return $this->automlModel;
  }
  /**
   * @param GoogleCloudAiplatformV1MigratableResourceDataLabelingDataset
   */
  public function setDataLabelingDataset(GoogleCloudAiplatformV1MigratableResourceDataLabelingDataset $dataLabelingDataset)
  {
    $this->dataLabelingDataset = $dataLabelingDataset;
  }
  /**
   * @return GoogleCloudAiplatformV1MigratableResourceDataLabelingDataset
   */
  public function getDataLabelingDataset()
  {
    return $this->dataLabelingDataset;
  }
  /**
   * @param string
   */
  public function setLastMigrateTime($lastMigrateTime)
  {
    $this->lastMigrateTime = $lastMigrateTime;
  }
  /**
   * @return string
   */
  public function getLastMigrateTime()
  {
    return $this->lastMigrateTime;
  }
  /**
   * @param string
   */
  public function setLastUpdateTime($lastUpdateTime)
  {
    $this->lastUpdateTime = $lastUpdateTime;
  }
  /**
   * @return string
   */
  public function getLastUpdateTime()
  {
    return $this->lastUpdateTime;
  }
  /**
   * @param GoogleCloudAiplatformV1MigratableResourceMlEngineModelVersion
   */
  public function setMlEngineModelVersion(GoogleCloudAiplatformV1MigratableResourceMlEngineModelVersion $mlEngineModelVersion)
  {
    $this->mlEngineModelVersion = $mlEngineModelVersion;
  }
  /**
   * @return GoogleCloudAiplatformV1MigratableResourceMlEngineModelVersion
   */
  public function getMlEngineModelVersion()
  {
    return $this->mlEngineModelVersion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1MigratableResource::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1MigratableResource');
