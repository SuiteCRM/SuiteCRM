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

namespace Google\Service\Pubsub;

class Topic extends \Google\Collection
{
  protected $collection_key = 'messageTransforms';
  protected $ingestionDataSourceSettingsType = IngestionDataSourceSettings::class;
  protected $ingestionDataSourceSettingsDataType = '';
  /**
   * @var string
   */
  public $kmsKeyName;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $messageRetentionDuration;
  protected $messageStoragePolicyType = MessageStoragePolicy::class;
  protected $messageStoragePolicyDataType = '';
  protected $messageTransformsType = MessageTransform::class;
  protected $messageTransformsDataType = 'array';
  /**
   * @var string
   */
  public $name;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  protected $schemaSettingsType = SchemaSettings::class;
  protected $schemaSettingsDataType = '';
  /**
   * @var string
   */
  public $state;

  /**
   * @param IngestionDataSourceSettings
   */
  public function setIngestionDataSourceSettings(IngestionDataSourceSettings $ingestionDataSourceSettings)
  {
    $this->ingestionDataSourceSettings = $ingestionDataSourceSettings;
  }
  /**
   * @return IngestionDataSourceSettings
   */
  public function getIngestionDataSourceSettings()
  {
    return $this->ingestionDataSourceSettings;
  }
  /**
   * @param string
   */
  public function setKmsKeyName($kmsKeyName)
  {
    $this->kmsKeyName = $kmsKeyName;
  }
  /**
   * @return string
   */
  public function getKmsKeyName()
  {
    return $this->kmsKeyName;
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
   * @param string
   */
  public function setMessageRetentionDuration($messageRetentionDuration)
  {
    $this->messageRetentionDuration = $messageRetentionDuration;
  }
  /**
   * @return string
   */
  public function getMessageRetentionDuration()
  {
    return $this->messageRetentionDuration;
  }
  /**
   * @param MessageStoragePolicy
   */
  public function setMessageStoragePolicy(MessageStoragePolicy $messageStoragePolicy)
  {
    $this->messageStoragePolicy = $messageStoragePolicy;
  }
  /**
   * @return MessageStoragePolicy
   */
  public function getMessageStoragePolicy()
  {
    return $this->messageStoragePolicy;
  }
  /**
   * @param MessageTransform[]
   */
  public function setMessageTransforms($messageTransforms)
  {
    $this->messageTransforms = $messageTransforms;
  }
  /**
   * @return MessageTransform[]
   */
  public function getMessageTransforms()
  {
    return $this->messageTransforms;
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
   * @param SchemaSettings
   */
  public function setSchemaSettings(SchemaSettings $schemaSettings)
  {
    $this->schemaSettings = $schemaSettings;
  }
  /**
   * @return SchemaSettings
   */
  public function getSchemaSettings()
  {
    return $this->schemaSettings;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Topic::class, 'Google_Service_Pubsub_Topic');
