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

namespace Google\Service\ChecksService;

class GoogleChecksRepoScanV1alphaRepoScan extends \Google\Collection
{
  protected $collection_key = 'sources';
  /**
   * @var string
   */
  public $cliVersion;
  /**
   * @var string
   */
  public $localScanPath;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $resultsUri;
  protected $scmMetadataType = GoogleChecksRepoScanV1alphaScmMetadata::class;
  protected $scmMetadataDataType = '';
  protected $sourcesType = GoogleChecksRepoScanV1alphaSource::class;
  protected $sourcesDataType = 'array';

  /**
   * @param string
   */
  public function setCliVersion($cliVersion)
  {
    $this->cliVersion = $cliVersion;
  }
  /**
   * @return string
   */
  public function getCliVersion()
  {
    return $this->cliVersion;
  }
  /**
   * @param string
   */
  public function setLocalScanPath($localScanPath)
  {
    $this->localScanPath = $localScanPath;
  }
  /**
   * @return string
   */
  public function getLocalScanPath()
  {
    return $this->localScanPath;
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
   * @param string
   */
  public function setResultsUri($resultsUri)
  {
    $this->resultsUri = $resultsUri;
  }
  /**
   * @return string
   */
  public function getResultsUri()
  {
    return $this->resultsUri;
  }
  /**
   * @param GoogleChecksRepoScanV1alphaScmMetadata
   */
  public function setScmMetadata(GoogleChecksRepoScanV1alphaScmMetadata $scmMetadata)
  {
    $this->scmMetadata = $scmMetadata;
  }
  /**
   * @return GoogleChecksRepoScanV1alphaScmMetadata
   */
  public function getScmMetadata()
  {
    return $this->scmMetadata;
  }
  /**
   * @param GoogleChecksRepoScanV1alphaSource[]
   */
  public function setSources($sources)
  {
    $this->sources = $sources;
  }
  /**
   * @return GoogleChecksRepoScanV1alphaSource[]
   */
  public function getSources()
  {
    return $this->sources;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChecksRepoScanV1alphaRepoScan::class, 'Google_Service_ChecksService_GoogleChecksRepoScanV1alphaRepoScan');
