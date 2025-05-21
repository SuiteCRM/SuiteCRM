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

class GoogleChecksReportV1alphaCheck extends \Google\Collection
{
  protected $collection_key = 'regionCodes';
  protected $citationsType = GoogleChecksReportV1alphaCheckCitation::class;
  protected $citationsDataType = 'array';
  protected $evidenceType = GoogleChecksReportV1alphaCheckEvidence::class;
  protected $evidenceDataType = '';
  /**
   * @var string[]
   */
  public $regionCodes;
  /**
   * @var string
   */
  public $severity;
  /**
   * @var string
   */
  public $state;
  protected $stateMetadataType = GoogleChecksReportV1alphaCheckStateMetadata::class;
  protected $stateMetadataDataType = '';
  /**
   * @var string
   */
  public $type;

  /**
   * @param GoogleChecksReportV1alphaCheckCitation[]
   */
  public function setCitations($citations)
  {
    $this->citations = $citations;
  }
  /**
   * @return GoogleChecksReportV1alphaCheckCitation[]
   */
  public function getCitations()
  {
    return $this->citations;
  }
  /**
   * @param GoogleChecksReportV1alphaCheckEvidence
   */
  public function setEvidence(GoogleChecksReportV1alphaCheckEvidence $evidence)
  {
    $this->evidence = $evidence;
  }
  /**
   * @return GoogleChecksReportV1alphaCheckEvidence
   */
  public function getEvidence()
  {
    return $this->evidence;
  }
  /**
   * @param string[]
   */
  public function setRegionCodes($regionCodes)
  {
    $this->regionCodes = $regionCodes;
  }
  /**
   * @return string[]
   */
  public function getRegionCodes()
  {
    return $this->regionCodes;
  }
  /**
   * @param string
   */
  public function setSeverity($severity)
  {
    $this->severity = $severity;
  }
  /**
   * @return string
   */
  public function getSeverity()
  {
    return $this->severity;
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
   * @param GoogleChecksReportV1alphaCheckStateMetadata
   */
  public function setStateMetadata(GoogleChecksReportV1alphaCheckStateMetadata $stateMetadata)
  {
    $this->stateMetadata = $stateMetadata;
  }
  /**
   * @return GoogleChecksReportV1alphaCheckStateMetadata
   */
  public function getStateMetadata()
  {
    return $this->stateMetadata;
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
class_alias(GoogleChecksReportV1alphaCheck::class, 'Google_Service_ChecksService_GoogleChecksReportV1alphaCheck');
