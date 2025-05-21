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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1betaDataStore extends \Google\Collection
{
  protected $collection_key = 'solutionTypes';
  protected $advancedSiteSearchConfigType = GoogleCloudDiscoveryengineV1betaAdvancedSiteSearchConfig::class;
  protected $advancedSiteSearchConfigDataType = '';
  protected $billingEstimationType = GoogleCloudDiscoveryengineV1betaDataStoreBillingEstimation::class;
  protected $billingEstimationDataType = '';
  protected $cmekConfigType = GoogleCloudDiscoveryengineV1betaCmekConfig::class;
  protected $cmekConfigDataType = '';
  /**
   * @var string
   */
  public $contentConfig;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $defaultSchemaId;
  /**
   * @var string
   */
  public $displayName;
  protected $documentProcessingConfigType = GoogleCloudDiscoveryengineV1betaDocumentProcessingConfig::class;
  protected $documentProcessingConfigDataType = '';
  protected $healthcareFhirConfigType = GoogleCloudDiscoveryengineV1betaHealthcareFhirConfig::class;
  protected $healthcareFhirConfigDataType = '';
  /**
   * @var string
   */
  public $identityMappingStore;
  /**
   * @var string
   */
  public $industryVertical;
  /**
   * @var bool
   */
  public $isInfobotFaqDataStore;
  /**
   * @var string
   */
  public $kmsKeyName;
  protected $languageInfoType = GoogleCloudDiscoveryengineV1betaLanguageInfo::class;
  protected $languageInfoDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $naturalLanguageQueryUnderstandingConfigType = GoogleCloudDiscoveryengineV1betaNaturalLanguageQueryUnderstandingConfig::class;
  protected $naturalLanguageQueryUnderstandingConfigDataType = '';
  protected $servingConfigDataStoreType = GoogleCloudDiscoveryengineV1betaDataStoreServingConfigDataStore::class;
  protected $servingConfigDataStoreDataType = '';
  /**
   * @var string[]
   */
  public $solutionTypes;
  protected $startingSchemaType = GoogleCloudDiscoveryengineV1betaSchema::class;
  protected $startingSchemaDataType = '';
  protected $workspaceConfigType = GoogleCloudDiscoveryengineV1betaWorkspaceConfig::class;
  protected $workspaceConfigDataType = '';

  /**
   * @param GoogleCloudDiscoveryengineV1betaAdvancedSiteSearchConfig
   */
  public function setAdvancedSiteSearchConfig(GoogleCloudDiscoveryengineV1betaAdvancedSiteSearchConfig $advancedSiteSearchConfig)
  {
    $this->advancedSiteSearchConfig = $advancedSiteSearchConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaAdvancedSiteSearchConfig
   */
  public function getAdvancedSiteSearchConfig()
  {
    return $this->advancedSiteSearchConfig;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaDataStoreBillingEstimation
   */
  public function setBillingEstimation(GoogleCloudDiscoveryengineV1betaDataStoreBillingEstimation $billingEstimation)
  {
    $this->billingEstimation = $billingEstimation;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaDataStoreBillingEstimation
   */
  public function getBillingEstimation()
  {
    return $this->billingEstimation;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaCmekConfig
   */
  public function setCmekConfig(GoogleCloudDiscoveryengineV1betaCmekConfig $cmekConfig)
  {
    $this->cmekConfig = $cmekConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaCmekConfig
   */
  public function getCmekConfig()
  {
    return $this->cmekConfig;
  }
  /**
   * @param string
   */
  public function setContentConfig($contentConfig)
  {
    $this->contentConfig = $contentConfig;
  }
  /**
   * @return string
   */
  public function getContentConfig()
  {
    return $this->contentConfig;
  }
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
   * @param string
   */
  public function setDefaultSchemaId($defaultSchemaId)
  {
    $this->defaultSchemaId = $defaultSchemaId;
  }
  /**
   * @return string
   */
  public function getDefaultSchemaId()
  {
    return $this->defaultSchemaId;
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
   * @param GoogleCloudDiscoveryengineV1betaDocumentProcessingConfig
   */
  public function setDocumentProcessingConfig(GoogleCloudDiscoveryengineV1betaDocumentProcessingConfig $documentProcessingConfig)
  {
    $this->documentProcessingConfig = $documentProcessingConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaDocumentProcessingConfig
   */
  public function getDocumentProcessingConfig()
  {
    return $this->documentProcessingConfig;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaHealthcareFhirConfig
   */
  public function setHealthcareFhirConfig(GoogleCloudDiscoveryengineV1betaHealthcareFhirConfig $healthcareFhirConfig)
  {
    $this->healthcareFhirConfig = $healthcareFhirConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaHealthcareFhirConfig
   */
  public function getHealthcareFhirConfig()
  {
    return $this->healthcareFhirConfig;
  }
  /**
   * @param string
   */
  public function setIdentityMappingStore($identityMappingStore)
  {
    $this->identityMappingStore = $identityMappingStore;
  }
  /**
   * @return string
   */
  public function getIdentityMappingStore()
  {
    return $this->identityMappingStore;
  }
  /**
   * @param string
   */
  public function setIndustryVertical($industryVertical)
  {
    $this->industryVertical = $industryVertical;
  }
  /**
   * @return string
   */
  public function getIndustryVertical()
  {
    return $this->industryVertical;
  }
  /**
   * @param bool
   */
  public function setIsInfobotFaqDataStore($isInfobotFaqDataStore)
  {
    $this->isInfobotFaqDataStore = $isInfobotFaqDataStore;
  }
  /**
   * @return bool
   */
  public function getIsInfobotFaqDataStore()
  {
    return $this->isInfobotFaqDataStore;
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
   * @param GoogleCloudDiscoveryengineV1betaLanguageInfo
   */
  public function setLanguageInfo(GoogleCloudDiscoveryengineV1betaLanguageInfo $languageInfo)
  {
    $this->languageInfo = $languageInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaLanguageInfo
   */
  public function getLanguageInfo()
  {
    return $this->languageInfo;
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
   * @param GoogleCloudDiscoveryengineV1betaNaturalLanguageQueryUnderstandingConfig
   */
  public function setNaturalLanguageQueryUnderstandingConfig(GoogleCloudDiscoveryengineV1betaNaturalLanguageQueryUnderstandingConfig $naturalLanguageQueryUnderstandingConfig)
  {
    $this->naturalLanguageQueryUnderstandingConfig = $naturalLanguageQueryUnderstandingConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaNaturalLanguageQueryUnderstandingConfig
   */
  public function getNaturalLanguageQueryUnderstandingConfig()
  {
    return $this->naturalLanguageQueryUnderstandingConfig;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaDataStoreServingConfigDataStore
   */
  public function setServingConfigDataStore(GoogleCloudDiscoveryengineV1betaDataStoreServingConfigDataStore $servingConfigDataStore)
  {
    $this->servingConfigDataStore = $servingConfigDataStore;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaDataStoreServingConfigDataStore
   */
  public function getServingConfigDataStore()
  {
    return $this->servingConfigDataStore;
  }
  /**
   * @param string[]
   */
  public function setSolutionTypes($solutionTypes)
  {
    $this->solutionTypes = $solutionTypes;
  }
  /**
   * @return string[]
   */
  public function getSolutionTypes()
  {
    return $this->solutionTypes;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaSchema
   */
  public function setStartingSchema(GoogleCloudDiscoveryengineV1betaSchema $startingSchema)
  {
    $this->startingSchema = $startingSchema;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaSchema
   */
  public function getStartingSchema()
  {
    return $this->startingSchema;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaWorkspaceConfig
   */
  public function setWorkspaceConfig(GoogleCloudDiscoveryengineV1betaWorkspaceConfig $workspaceConfig)
  {
    $this->workspaceConfig = $workspaceConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaWorkspaceConfig
   */
  public function getWorkspaceConfig()
  {
    return $this->workspaceConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1betaDataStore::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1betaDataStore');
