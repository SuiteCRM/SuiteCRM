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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV2Cve extends \Google\Collection
{
  protected $collection_key = 'references';
  protected $cvssv3Type = GoogleCloudSecuritycenterV2Cvssv3::class;
  protected $cvssv3DataType = '';
  /**
   * @var string
   */
  public $exploitReleaseDate;
  /**
   * @var string
   */
  public $exploitationActivity;
  /**
   * @var string
   */
  public $firstExploitationDate;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $impact;
  /**
   * @var bool
   */
  public $observedInTheWild;
  protected $referencesType = GoogleCloudSecuritycenterV2Reference::class;
  protected $referencesDataType = 'array';
  /**
   * @var bool
   */
  public $upstreamFixAvailable;
  /**
   * @var bool
   */
  public $zeroDay;

  /**
   * @param GoogleCloudSecuritycenterV2Cvssv3
   */
  public function setCvssv3(GoogleCloudSecuritycenterV2Cvssv3 $cvssv3)
  {
    $this->cvssv3 = $cvssv3;
  }
  /**
   * @return GoogleCloudSecuritycenterV2Cvssv3
   */
  public function getCvssv3()
  {
    return $this->cvssv3;
  }
  /**
   * @param string
   */
  public function setExploitReleaseDate($exploitReleaseDate)
  {
    $this->exploitReleaseDate = $exploitReleaseDate;
  }
  /**
   * @return string
   */
  public function getExploitReleaseDate()
  {
    return $this->exploitReleaseDate;
  }
  /**
   * @param string
   */
  public function setExploitationActivity($exploitationActivity)
  {
    $this->exploitationActivity = $exploitationActivity;
  }
  /**
   * @return string
   */
  public function getExploitationActivity()
  {
    return $this->exploitationActivity;
  }
  /**
   * @param string
   */
  public function setFirstExploitationDate($firstExploitationDate)
  {
    $this->firstExploitationDate = $firstExploitationDate;
  }
  /**
   * @return string
   */
  public function getFirstExploitationDate()
  {
    return $this->firstExploitationDate;
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
   * @param string
   */
  public function setImpact($impact)
  {
    $this->impact = $impact;
  }
  /**
   * @return string
   */
  public function getImpact()
  {
    return $this->impact;
  }
  /**
   * @param bool
   */
  public function setObservedInTheWild($observedInTheWild)
  {
    $this->observedInTheWild = $observedInTheWild;
  }
  /**
   * @return bool
   */
  public function getObservedInTheWild()
  {
    return $this->observedInTheWild;
  }
  /**
   * @param GoogleCloudSecuritycenterV2Reference[]
   */
  public function setReferences($references)
  {
    $this->references = $references;
  }
  /**
   * @return GoogleCloudSecuritycenterV2Reference[]
   */
  public function getReferences()
  {
    return $this->references;
  }
  /**
   * @param bool
   */
  public function setUpstreamFixAvailable($upstreamFixAvailable)
  {
    $this->upstreamFixAvailable = $upstreamFixAvailable;
  }
  /**
   * @return bool
   */
  public function getUpstreamFixAvailable()
  {
    return $this->upstreamFixAvailable;
  }
  /**
   * @param bool
   */
  public function setZeroDay($zeroDay)
  {
    $this->zeroDay = $zeroDay;
  }
  /**
   * @return bool
   */
  public function getZeroDay()
  {
    return $this->zeroDay;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2Cve::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2Cve');
