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

class GoogleCloudDiscoveryengineV1alphaDataStore extends \Google\Collection
{
  protected $collection_key = 'solutionTypes';
  /**
   * @var bool
   */
  public $aclEnabled;
  protected $advancedSiteSearchConfigType = GoogleCloudDiscoveryengineV1alphaAdvancedSiteSearchConfig::class;
  protected $advancedSiteSearchConfigDataType = '';
  protected $billingEstimationType = GoogleCloudDiscoveryengineV1alphaDataStoreBillingEstimation::class;
  protected $billingEstimationDataType = '';
  protected $cmekConfigType = GoogleCloudDiscoveryengineV1alphaCmekConfig::class;
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
  protected $documentProcessingConfigType = GoogleCloudDiscoveryengineV1alphaDocumentProcessingConfig::class;
  protected $documentProcessingConfigDataType = '';
  protected $healthcareFhirConfigType = GoogleCloudDiscoveryengineV1alphaHealthcareFhirConfig::class;
  protected $healthcareFhirConfigDataType = '';
  /**
   * @var string
   */
  public $identityMappingStore;
  protected $idpConfigType = GoogleCloudDiscoveryengineV1alphaIdpConfig::class;
  protected $idpConfigDataType = '';
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
  protected $languageInfoType = GoogleCloudDiscoveryengineV1alphaLanguageInfo::class;
  protected $languageInfoDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $naturalLanguageQueryUnderstandingConfigType = GoogleCloudDiscoveryengineV1alphaNaturalLanguageQueryUnderstandingConfig::class;
  protected $naturalLanguageQueryUnderstandingConfigDataType = '';
  protected $servingConfigDataStoreType = GoogleCloudDiscoveryengineV1alphaDataStoreServingConfigDataStore::class;
  protected $servingConfigDataStoreDataType = '';
  /**
   * @var string[]
   */
  public $solutionTypes;
  protected $startingSchemaType = GoogleCloudDiscoveryengineV1alphaSchema::class;
  protected $startingSchemaDataType = '';
  protected $workspaceConfigType = GoogleCloudDiscoveryengineV1alphaWorkspaceConfig::class;
  protected $workspaceConfigDataType = '';

  /**
   * @param bool
   */
  public function setAclEnabled($aclEnabled)
  {
    $this->aclEnabled = $aclEnabled;
  }
  /**
   * @return bool
   */
  public function getAclEnabled()
  {
    return $this->aclEnabled;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaAdvancedSiteSearchConfig
   */
  public function setAdvancedSiteSearchConfig(GoogleCloudDiscoveryengineV1alphaAdvancedSiteSearchConfig $advancedSiteSearchConfig)
  {
    $this->advancedSiteSearchConfig = $advancedSiteSearchConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaAdvancedSiteSearchConfig
   */
  public function getAdvancedSiteSearchConfig()
  {
    return $this->advancedSiteSearchConfig;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaDataStoreBillingEstimation
   */
  public function setBillingEstimation(GoogleCloudDiscoveryengineV1alphaDataStoreBillingEstimation $billingEstimation)
  {
    $this->billingEstimation = $billingEstimation;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaDataStoreBillingEstimation
   */
  public function getBillingEstimation()
  {
    return $this->billingEstimation;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaCmekConfig
   */
  public function setCmekConfig(GoogleCloudDiscoveryengineV1alphaCmekConfig $cmekConfig)
  {
    $this->cmekConfig = $cmekConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaCmekConfig
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
   * @param GoogleCloudDiscoveryengineV1alphaDocumentProcessingConfig
   */
  public function setDocumentProcessingConfig(GoogleCloudDiscoveryengineV1alphaDocumentProcessingConfig $documentProcessingConfig)
  {
    $this->documentProcessingConfig = $documentProcessingConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaDocumentProcessingConfig
   */
  public function getDocumentProcessingConfig()
  {
    return $this->documentProcessingConfig;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaHealthcareFhirConfig
   */
  public function setHealthcareFhirConfig(GoogleCloudDiscoveryengineV1alphaHealthcareFhirConfig $healthcareFhirConfig)
  {
    $this->healthcareFhirConfig = $healthcareFhirConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaHealthcareFhirConfig
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
   * @param GoogleCloudDiscoveryengineV1alphaIdpConfig
   */
  public function setIdpConfig(GoogleCloudDiscoveryengineV1alphaIdpConfig $idpConfig)
  {
    $this->idpConfig = $idpConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaIdpConfig
   */
  public function getIdpConfig()
  {
    return $this->idpConfig;
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
   * @param GoogleCloudDiscoveryengineV1alphaLanguageInfo
   */
  public function setLanguageInfo(GoogleCloudDiscoveryengineV1alphaLanguageInfo $languageInfo)
  {
    $this->languageInfo = $languageInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaLanguageInfo
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
   * @param GoogleCloudDiscoveryengineV1alphaNaturalLanguageQueryUnderstandingConfig
   */
  public function setNaturalLanguageQueryUnderstandingConfig(GoogleCloudDiscoveryengineV1alphaNaturalLanguageQueryUnderstandingConfig $naturalLanguageQueryUnderstandingConfig)
  {
    $this->naturalLanguageQueryUnderstandingConfig = $naturalLanguageQueryUnderstandingConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaNaturalLanguageQueryUnderstandingConfig
   */
  public function getNaturalLanguageQueryUnderstandingConfig()
  {
    return $this->naturalLanguageQueryUnderstandingConfig;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaDataStoreServingConfigDataStore
   */
  public function setServingConfigDataStore(GoogleCloudDiscoveryengineV1alphaDataStoreServingConfigDataStore $servingConfigDataStore)
  {
    $this->servingConfigDataStore = $servingConfigDataStore;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaDataStoreServingConfigDataStore
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
   * @param GoogleCloudDiscoveryengineV1alphaSchema
   */
  public function setStartingSchema(GoogleCloudDiscoveryengineV1alphaSchema $startingSchema)
  {
    $this->startingSchema = $startingSchema;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSchema
   */
  public function getStartingSchema()
  {
    return $this->startingSchema;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaWorkspaceConfig
   */
  public function setWorkspaceConfig(GoogleCloudDiscoveryengineV1alphaWorkspaceConfig $workspaceConfig)
  {
    $this->workspaceConfig = $workspaceConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaWorkspaceConfig
   */
  public function getWorkspaceConfig()
  {
    return $this->workspaceConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaDataStore::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaDataStore');
