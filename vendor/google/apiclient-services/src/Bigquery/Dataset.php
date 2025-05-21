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

namespace Google\Service\Bigquery;

class Dataset extends \Google\Collection
{
  protected $collection_key = 'tags';
  protected $accessType = DatasetAccess::class;
  protected $accessDataType = 'array';
  /**
   * @var string
   */
  public $creationTime;
  protected $datasetReferenceType = DatasetReference::class;
  protected $datasetReferenceDataType = '';
  /**
   * @var string
   */
  public $defaultCollation;
  protected $defaultEncryptionConfigurationType = EncryptionConfiguration::class;
  protected $defaultEncryptionConfigurationDataType = '';
  /**
   * @var string
   */
  public $defaultPartitionExpirationMs;
  /**
   * @var string
   */
  public $defaultRoundingMode;
  /**
   * @var string
   */
  public $defaultTableExpirationMs;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $etag;
  protected $externalCatalogDatasetOptionsType = ExternalCatalogDatasetOptions::class;
  protected $externalCatalogDatasetOptionsDataType = '';
  protected $externalDatasetReferenceType = ExternalDatasetReference::class;
  protected $externalDatasetReferenceDataType = '';
  /**
   * @var string
   */
  public $friendlyName;
  /**
   * @var string
   */
  public $id;
  /**
   * @var bool
   */
  public $isCaseInsensitive;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $lastModifiedTime;
  protected $linkedDatasetMetadataType = LinkedDatasetMetadata::class;
  protected $linkedDatasetMetadataDataType = '';
  protected $linkedDatasetSourceType = LinkedDatasetSource::class;
  protected $linkedDatasetSourceDataType = '';
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $maxTimeTravelHours;
  /**
   * @var string[]
   */
  public $resourceTags;
  protected $restrictionsType = RestrictionConfig::class;
  protected $restrictionsDataType = '';
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
  public $selfLink;
  /**
   * @var string
   */
  public $storageBillingModel;
  protected $tagsType = DatasetTags::class;
  protected $tagsDataType = 'array';
  /**
   * @var string
   */
  public $type;

  /**
   * @param DatasetAccess[]
   */
  public function setAccess($access)
  {
    $this->access = $access;
  }
  /**
   * @return DatasetAccess[]
   */
  public function getAccess()
  {
    return $this->access;
  }
  /**
   * @param string
   */
  public function setCreationTime($creationTime)
  {
    $this->creationTime = $creationTime;
  }
  /**
   * @return string
   */
  public function getCreationTime()
  {
    return $this->creationTime;
  }
  /**
   * @param DatasetReference
   */
  public function setDatasetReference(DatasetReference $datasetReference)
  {
    $this->datasetReference = $datasetReference;
  }
  /**
   * @return DatasetReference
   */
  public function getDatasetReference()
  {
    return $this->datasetReference;
  }
  /**
   * @param string
   */
  public function setDefaultCollation($defaultCollation)
  {
    $this->defaultCollation = $defaultCollation;
  }
  /**
   * @return string
   */
  public function getDefaultCollation()
  {
    return $this->defaultCollation;
  }
  /**
   * @param EncryptionConfiguration
   */
  public function setDefaultEncryptionConfiguration(EncryptionConfiguration $defaultEncryptionConfiguration)
  {
    $this->defaultEncryptionConfiguration = $defaultEncryptionConfiguration;
  }
  /**
   * @return EncryptionConfiguration
   */
  public function getDefaultEncryptionConfiguration()
  {
    return $this->defaultEncryptionConfiguration;
  }
  /**
   * @param string
   */
  public function setDefaultPartitionExpirationMs($defaultPartitionExpirationMs)
  {
    $this->defaultPartitionExpirationMs = $defaultPartitionExpirationMs;
  }
  /**
   * @return string
   */
  public function getDefaultPartitionExpirationMs()
  {
    return $this->defaultPartitionExpirationMs;
  }
  /**
   * @param string
   */
  public function setDefaultRoundingMode($defaultRoundingMode)
  {
    $this->defaultRoundingMode = $defaultRoundingMode;
  }
  /**
   * @return string
   */
  public function getDefaultRoundingMode()
  {
    return $this->defaultRoundingMode;
  }
  /**
   * @param string
   */
  public function setDefaultTableExpirationMs($defaultTableExpirationMs)
  {
    $this->defaultTableExpirationMs = $defaultTableExpirationMs;
  }
  /**
   * @return string
   */
  public function getDefaultTableExpirationMs()
  {
    return $this->defaultTableExpirationMs;
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
   * @param ExternalCatalogDatasetOptions
   */
  public function setExternalCatalogDatasetOptions(ExternalCatalogDatasetOptions $externalCatalogDatasetOptions)
  {
    $this->externalCatalogDatasetOptions = $externalCatalogDatasetOptions;
  }
  /**
   * @return ExternalCatalogDatasetOptions
   */
  public function getExternalCatalogDatasetOptions()
  {
    return $this->externalCatalogDatasetOptions;
  }
  /**
   * @param ExternalDatasetReference
   */
  public function setExternalDatasetReference(ExternalDatasetReference $externalDatasetReference)
  {
    $this->externalDatasetReference = $externalDatasetReference;
  }
  /**
   * @return ExternalDatasetReference
   */
  public function getExternalDatasetReference()
  {
    return $this->externalDatasetReference;
  }
  /**
   * @param string
   */
  public function setFriendlyName($friendlyName)
  {
    $this->friendlyName = $friendlyName;
  }
  /**
   * @return string
   */
  public function getFriendlyName()
  {
    return $this->friendlyName;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param bool
   */
  public function setIsCaseInsensitive($isCaseInsensitive)
  {
    $this->isCaseInsensitive = $isCaseInsensitive;
  }
  /**
   * @return bool
   */
  public function getIsCaseInsensitive()
  {
    return $this->isCaseInsensitive;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
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
  public function setLastModifiedTime($lastModifiedTime)
  {
    $this->lastModifiedTime = $lastModifiedTime;
  }
  /**
   * @return string
   */
  public function getLastModifiedTime()
  {
    return $this->lastModifiedTime;
  }
  /**
   * @param LinkedDatasetMetadata
   */
  public function setLinkedDatasetMetadata(LinkedDatasetMetadata $linkedDatasetMetadata)
  {
    $this->linkedDatasetMetadata = $linkedDatasetMetadata;
  }
  /**
   * @return LinkedDatasetMetadata
   */
  public function getLinkedDatasetMetadata()
  {
    return $this->linkedDatasetMetadata;
  }
  /**
   * @param LinkedDatasetSource
   */
  public function setLinkedDatasetSource(LinkedDatasetSource $linkedDatasetSource)
  {
    $this->linkedDatasetSource = $linkedDatasetSource;
  }
  /**
   * @return LinkedDatasetSource
   */
  public function getLinkedDatasetSource()
  {
    return $this->linkedDatasetSource;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param string
   */
  public function setMaxTimeTravelHours($maxTimeTravelHours)
  {
    $this->maxTimeTravelHours = $maxTimeTravelHours;
  }
  /**
   * @return string
   */
  public function getMaxTimeTravelHours()
  {
    return $this->maxTimeTravelHours;
  }
  /**
   * @param string[]
   */
  public function setResourceTags($resourceTags)
  {
    $this->resourceTags = $resourceTags;
  }
  /**
   * @return string[]
   */
  public function getResourceTags()
  {
    return $this->resourceTags;
  }
  /**
   * @param RestrictionConfig
   */
  public function setRestrictions(RestrictionConfig $restrictions)
  {
    $this->restrictions = $restrictions;
  }
  /**
   * @return RestrictionConfig
   */
  public function getRestrictions()
  {
    return $this->restrictions;
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
  public function setSelfLink($selfLink)
  {
    $this->selfLink = $selfLink;
  }
  /**
   * @return string
   */
  public function getSelfLink()
  {
    return $this->selfLink;
  }
  /**
   * @param string
   */
  public function setStorageBillingModel($storageBillingModel)
  {
    $this->storageBillingModel = $storageBillingModel;
  }
  /**
   * @return string
   */
  public function getStorageBillingModel()
  {
    return $this->storageBillingModel;
  }
  /**
   * @param DatasetTags[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return DatasetTags[]
   */
  public function getTags()
  {
    return $this->tags;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Dataset::class, 'Google_Service_Bigquery_Dataset');
