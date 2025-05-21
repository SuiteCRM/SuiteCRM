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

class GoogleCloudAiplatformV1Model extends \Google\Collection
{
  protected $collection_key = 'versionAliases';
  /**
   * @var string
   */
  public $artifactUri;
  protected $baseModelSourceType = GoogleCloudAiplatformV1ModelBaseModelSource::class;
  protected $baseModelSourceDataType = '';
  protected $containerSpecType = GoogleCloudAiplatformV1ModelContainerSpec::class;
  protected $containerSpecDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $dataStatsType = GoogleCloudAiplatformV1ModelDataStats::class;
  protected $dataStatsDataType = '';
  /**
   * @var string
   */
  public $defaultCheckpointId;
  protected $deployedModelsType = GoogleCloudAiplatformV1DeployedModelRef::class;
  protected $deployedModelsDataType = 'array';
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
  protected $explanationSpecType = GoogleCloudAiplatformV1ExplanationSpec::class;
  protected $explanationSpecDataType = '';
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
  public $metadataArtifact;
  /**
   * @var string
   */
  public $metadataSchemaUri;
  protected $modelSourceInfoType = GoogleCloudAiplatformV1ModelSourceInfo::class;
  protected $modelSourceInfoDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $originalModelInfoType = GoogleCloudAiplatformV1ModelOriginalModelInfo::class;
  protected $originalModelInfoDataType = '';
  /**
   * @var string
   */
  public $pipelineJob;
  protected $predictSchemataType = GoogleCloudAiplatformV1PredictSchemata::class;
  protected $predictSchemataDataType = '';
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string[]
   */
  public $supportedDeploymentResourcesTypes;
  protected $supportedExportFormatsType = GoogleCloudAiplatformV1ModelExportFormat::class;
  protected $supportedExportFormatsDataType = 'array';
  /**
   * @var string[]
   */
  public $supportedInputStorageFormats;
  /**
   * @var string[]
   */
  public $supportedOutputStorageFormats;
  /**
   * @var string
   */
  public $trainingPipeline;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string[]
   */
  public $versionAliases;
  /**
   * @var string
   */
  public $versionCreateTime;
  /**
   * @var string
   */
  public $versionDescription;
  /**
   * @var string
   */
  public $versionId;
  /**
   * @var string
   */
  public $versionUpdateTime;

  /**
   * @param string
   */
  public function setArtifactUri($artifactUri)
  {
    $this->artifactUri = $artifactUri;
  }
  /**
   * @return string
   */
  public function getArtifactUri()
  {
    return $this->artifactUri;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelBaseModelSource
   */
  public function setBaseModelSource(GoogleCloudAiplatformV1ModelBaseModelSource $baseModelSource)
  {
    $this->baseModelSource = $baseModelSource;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelBaseModelSource
   */
  public function getBaseModelSource()
  {
    return $this->baseModelSource;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelContainerSpec
   */
  public function setContainerSpec(GoogleCloudAiplatformV1ModelContainerSpec $containerSpec)
  {
    $this->containerSpec = $containerSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelContainerSpec
   */
  public function getContainerSpec()
  {
    return $this->containerSpec;
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
   * @param GoogleCloudAiplatformV1ModelDataStats
   */
  public function setDataStats(GoogleCloudAiplatformV1ModelDataStats $dataStats)
  {
    $this->dataStats = $dataStats;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelDataStats
   */
  public function getDataStats()
  {
    return $this->dataStats;
  }
  /**
   * @param string
   */
  public function setDefaultCheckpointId($defaultCheckpointId)
  {
    $this->defaultCheckpointId = $defaultCheckpointId;
  }
  /**
   * @return string
   */
  public function getDefaultCheckpointId()
  {
    return $this->defaultCheckpointId;
  }
  /**
   * @param GoogleCloudAiplatformV1DeployedModelRef[]
   */
  public function setDeployedModels($deployedModels)
  {
    $this->deployedModels = $deployedModels;
  }
  /**
   * @return GoogleCloudAiplatformV1DeployedModelRef[]
   */
  public function getDeployedModels()
  {
    return $this->deployedModels;
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
   * @param GoogleCloudAiplatformV1ExplanationSpec
   */
  public function setExplanationSpec(GoogleCloudAiplatformV1ExplanationSpec $explanationSpec)
  {
    $this->explanationSpec = $explanationSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1ExplanationSpec
   */
  public function getExplanationSpec()
  {
    return $this->explanationSpec;
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
  public function setMetadataArtifact($metadataArtifact)
  {
    $this->metadataArtifact = $metadataArtifact;
  }
  /**
   * @return string
   */
  public function getMetadataArtifact()
  {
    return $this->metadataArtifact;
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
   * @param GoogleCloudAiplatformV1ModelSourceInfo
   */
  public function setModelSourceInfo(GoogleCloudAiplatformV1ModelSourceInfo $modelSourceInfo)
  {
    $this->modelSourceInfo = $modelSourceInfo;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelSourceInfo
   */
  public function getModelSourceInfo()
  {
    return $this->modelSourceInfo;
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
   * @param GoogleCloudAiplatformV1ModelOriginalModelInfo
   */
  public function setOriginalModelInfo(GoogleCloudAiplatformV1ModelOriginalModelInfo $originalModelInfo)
  {
    $this->originalModelInfo = $originalModelInfo;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelOriginalModelInfo
   */
  public function getOriginalModelInfo()
  {
    return $this->originalModelInfo;
  }
  /**
   * @param string
   */
  public function setPipelineJob($pipelineJob)
  {
    $this->pipelineJob = $pipelineJob;
  }
  /**
   * @return string
   */
  public function getPipelineJob()
  {
    return $this->pipelineJob;
  }
  /**
   * @param GoogleCloudAiplatformV1PredictSchemata
   */
  public function setPredictSchemata(GoogleCloudAiplatformV1PredictSchemata $predictSchemata)
  {
    $this->predictSchemata = $predictSchemata;
  }
  /**
   * @return GoogleCloudAiplatformV1PredictSchemata
   */
  public function getPredictSchemata()
  {
    return $this->predictSchemata;
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
   * @param string[]
   */
  public function setSupportedDeploymentResourcesTypes($supportedDeploymentResourcesTypes)
  {
    $this->supportedDeploymentResourcesTypes = $supportedDeploymentResourcesTypes;
  }
  /**
   * @return string[]
   */
  public function getSupportedDeploymentResourcesTypes()
  {
    return $this->supportedDeploymentResourcesTypes;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelExportFormat[]
   */
  public function setSupportedExportFormats($supportedExportFormats)
  {
    $this->supportedExportFormats = $supportedExportFormats;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelExportFormat[]
   */
  public function getSupportedExportFormats()
  {
    return $this->supportedExportFormats;
  }
  /**
   * @param string[]
   */
  public function setSupportedInputStorageFormats($supportedInputStorageFormats)
  {
    $this->supportedInputStorageFormats = $supportedInputStorageFormats;
  }
  /**
   * @return string[]
   */
  public function getSupportedInputStorageFormats()
  {
    return $this->supportedInputStorageFormats;
  }
  /**
   * @param string[]
   */
  public function setSupportedOutputStorageFormats($supportedOutputStorageFormats)
  {
    $this->supportedOutputStorageFormats = $supportedOutputStorageFormats;
  }
  /**
   * @return string[]
   */
  public function getSupportedOutputStorageFormats()
  {
    return $this->supportedOutputStorageFormats;
  }
  /**
   * @param string
   */
  public function setTrainingPipeline($trainingPipeline)
  {
    $this->trainingPipeline = $trainingPipeline;
  }
  /**
   * @return string
   */
  public function getTrainingPipeline()
  {
    return $this->trainingPipeline;
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
  /**
   * @param string[]
   */
  public function setVersionAliases($versionAliases)
  {
    $this->versionAliases = $versionAliases;
  }
  /**
   * @return string[]
   */
  public function getVersionAliases()
  {
    return $this->versionAliases;
  }
  /**
   * @param string
   */
  public function setVersionCreateTime($versionCreateTime)
  {
    $this->versionCreateTime = $versionCreateTime;
  }
  /**
   * @return string
   */
  public function getVersionCreateTime()
  {
    return $this->versionCreateTime;
  }
  /**
   * @param string
   */
  public function setVersionDescription($versionDescription)
  {
    $this->versionDescription = $versionDescription;
  }
  /**
   * @return string
   */
  public function getVersionDescription()
  {
    return $this->versionDescription;
  }
  /**
   * @param string
   */
  public function setVersionId($versionId)
  {
    $this->versionId = $versionId;
  }
  /**
   * @return string
   */
  public function getVersionId()
  {
    return $this->versionId;
  }
  /**
   * @param string
   */
  public function setVersionUpdateTime($versionUpdateTime)
  {
    $this->versionUpdateTime = $versionUpdateTime;
  }
  /**
   * @return string
   */
  public function getVersionUpdateTime()
  {
    return $this->versionUpdateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1Model::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1Model');
