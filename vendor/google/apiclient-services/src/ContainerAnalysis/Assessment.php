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

namespace Google\Service\ContainerAnalysis;

class Assessment extends \Google\Collection
{
  protected $collection_key = 'remediations';
  /**
   * @var string
   */
  public $cve;
  /**
   * @var string[]
   */
  public $impacts;
  protected $justificationType = Justification::class;
  protected $justificationDataType = '';
  /**
   * @var string
   */
  public $longDescription;
  protected $relatedUrisType = RelatedUrl::class;
  protected $relatedUrisDataType = 'array';
  protected $remediationsType = Remediation::class;
  protected $remediationsDataType = 'array';
  /**
   * @var string
   */
  public $shortDescription;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $vulnerabilityId;

  /**
   * @param string
   */
  public function setCve($cve)
  {
    $this->cve = $cve;
  }
  /**
   * @return string
   */
  public function getCve()
  {
    return $this->cve;
  }
  /**
   * @param string[]
   */
  public function setImpacts($impacts)
  {
    $this->impacts = $impacts;
  }
  /**
   * @return string[]
   */
  public function getImpacts()
  {
    return $this->impacts;
  }
  /**
   * @param Justification
   */
  public function setJustification(Justification $justification)
  {
    $this->justification = $justification;
  }
  /**
   * @return Justification
   */
  public function getJustification()
  {
    return $this->justification;
  }
  /**
   * @param string
   */
  public function setLongDescription($longDescription)
  {
    $this->longDescription = $longDescription;
  }
  /**
   * @return string
   */
  public function getLongDescription()
  {
    return $this->longDescription;
  }
  /**
   * @param RelatedUrl[]
   */
  public function setRelatedUris($relatedUris)
  {
    $this->relatedUris = $relatedUris;
  }
  /**
   * @return RelatedUrl[]
   */
  public function getRelatedUris()
  {
    return $this->relatedUris;
  }
  /**
   * @param Remediation[]
   */
  public function setRemediations($remediations)
  {
    $this->remediations = $remediations;
  }
  /**
   * @return Remediation[]
   */
  public function getRemediations()
  {
    return $this->remediations;
  }
  /**
   * @param string
   */
  public function setShortDescription($shortDescription)
  {
    $this->shortDescription = $shortDescription;
  }
  /**
   * @return string
   */
  public function getShortDescription()
  {
    return $this->shortDescription;
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
   * @param string
   */
  public function setVulnerabilityId($vulnerabilityId)
  {
    $this->vulnerabilityId = $vulnerabilityId;
  }
  /**
   * @return string
   */
  public function getVulnerabilityId()
  {
    return $this->vulnerabilityId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Assessment::class, 'Google_Service_ContainerAnalysis_Assessment');
