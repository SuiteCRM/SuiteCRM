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

namespace Google\Service\Contentwarehouse;

class GoogleCloudContentwarehouseV1GcsIngestWithDocAiProcessorsPipeline extends \Google\Collection
{
  protected $collection_key = 'extractProcessorInfos';
  protected $extractProcessorInfosType = GoogleCloudContentwarehouseV1ProcessorInfo::class;
  protected $extractProcessorInfosDataType = 'array';
  /**
   * @var string
   */
  public $inputPath;
  protected $pipelineConfigType = GoogleCloudContentwarehouseV1IngestPipelineConfig::class;
  protected $pipelineConfigDataType = '';
  /**
   * @var string
   */
  public $processorResultsFolderPath;
  /**
   * @var bool
   */
  public $skipIngestedDocuments;
  protected $splitClassifyProcessorInfoType = GoogleCloudContentwarehouseV1ProcessorInfo::class;
  protected $splitClassifyProcessorInfoDataType = '';

  /**
   * @param GoogleCloudContentwarehouseV1ProcessorInfo[]
   */
  public function setExtractProcessorInfos($extractProcessorInfos)
  {
    $this->extractProcessorInfos = $extractProcessorInfos;
  }
  /**
   * @return GoogleCloudContentwarehouseV1ProcessorInfo[]
   */
  public function getExtractProcessorInfos()
  {
    return $this->extractProcessorInfos;
  }
  /**
   * @param string
   */
  public function setInputPath($inputPath)
  {
    $this->inputPath = $inputPath;
  }
  /**
   * @return string
   */
  public function getInputPath()
  {
    return $this->inputPath;
  }
  /**
   * @param GoogleCloudContentwarehouseV1IngestPipelineConfig
   */
  public function setPipelineConfig(GoogleCloudContentwarehouseV1IngestPipelineConfig $pipelineConfig)
  {
    $this->pipelineConfig = $pipelineConfig;
  }
  /**
   * @return GoogleCloudContentwarehouseV1IngestPipelineConfig
   */
  public function getPipelineConfig()
  {
    return $this->pipelineConfig;
  }
  /**
   * @param string
   */
  public function setProcessorResultsFolderPath($processorResultsFolderPath)
  {
    $this->processorResultsFolderPath = $processorResultsFolderPath;
  }
  /**
   * @return string
   */
  public function getProcessorResultsFolderPath()
  {
    return $this->processorResultsFolderPath;
  }
  /**
   * @param bool
   */
  public function setSkipIngestedDocuments($skipIngestedDocuments)
  {
    $this->skipIngestedDocuments = $skipIngestedDocuments;
  }
  /**
   * @return bool
   */
  public function getSkipIngestedDocuments()
  {
    return $this->skipIngestedDocuments;
  }
  /**
   * @param GoogleCloudContentwarehouseV1ProcessorInfo
   */
  public function setSplitClassifyProcessorInfo(GoogleCloudContentwarehouseV1ProcessorInfo $splitClassifyProcessorInfo)
  {
    $this->splitClassifyProcessorInfo = $splitClassifyProcessorInfo;
  }
  /**
   * @return GoogleCloudContentwarehouseV1ProcessorInfo
   */
  public function getSplitClassifyProcessorInfo()
  {
    return $this->splitClassifyProcessorInfo;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContentwarehouseV1GcsIngestWithDocAiProcessorsPipeline::class, 'Google_Service_Contentwarehouse_GoogleCloudContentwarehouseV1GcsIngestWithDocAiProcessorsPipeline');
