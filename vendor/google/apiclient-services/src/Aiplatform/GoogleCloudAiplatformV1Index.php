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

class GoogleCloudAiplatformV1Index extends \Google\Collection
{
  protected $collection_key = 'deployedIndexes';
  /**
   * @var string
   */
  public $createTime;
  protected $deployedIndexesType = GoogleCloudAiplatformV1DeployedIndexRef::class;
  protected $deployedIndexesDataType = 'array';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  protected $encryptionSpecType = GoogleCloudAiplatformV1EncryptionSpec::class;
  protected $encryptionSpecDataType = '';
  /**
   * @var string
   */
  public $etag;
  protected $indexStatsType = GoogleCloudAiplatformV1IndexStats::class;
  protected $indexStatsDataType = '';
  /**
   * @var string
   */
  public $indexUpdateMethod;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var array
   */
  public $metadata;
  /**
   * @var string
   */
  public $metadataSchemaUri;
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
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param GoogleCloudAiplatformV1DeployedIndexRef[]
   */
  public function setDeployedIndexes($deployedIndexes)
  {
    $this->deployedIndexes = $deployedIndexes;
  }
  /**
   * @return GoogleCloudAiplatformV1DeployedIndexRef[]
   */
  public function getDeployedIndexes()
  {
    return $this->deployedIndexes;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param GoogleCloudAiplatformV1EncryptionSpec
   */
  public function setEncryptionSpec(GoogleCloudAiplatformV1EncryptionSpec $encryptionSpec)
  {
    $this->encryptionSpec = $encryptionSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1EncryptionSpec
   */
  public function getEncryptionSpec()
  {
    return $this->encryptionSpec;
  }
  /**
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
  }
  /**
   * @param GoogleCloudAiplatformV1IndexStats
   */
  public function setIndexStats(GoogleCloudAiplatformV1IndexStats $indexStats)
  {
    $this->indexStats = $indexStats;
  }
  /**
   * @return GoogleCloudAiplatformV1IndexStats
   */
  public function getIndexStats()
  {
    return $this->indexStats;
  }
  /**
   * @param string
   */
  public function setIndexUpdateMethod($indexUpdateMethod)
  {
    $this->indexUpdateMethod = $indexUpdateMethod;
  }
  /**
   * @return string
   */
  public function getIndexUpdateMethod()
  {
    return $this->indexUpdateMethod;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param array
   */
  public function setMetadata($metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return array
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param string
   */
  public function setMetadataSchemaUri($metadataSchemaUri)
  {
    $this->metadataSchemaUri = $metadataSchemaUri;
  }
  /**
   * @return string
   */
  public function getMetadataSchemaUri()
  {
    return $this->metadataSchemaUri;
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
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1Index::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1Index');
