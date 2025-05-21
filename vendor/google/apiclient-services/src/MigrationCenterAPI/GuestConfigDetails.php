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

namespace Google\Service\MigrationCenterAPI;

class GuestConfigDetails extends \Google\Model
{
  protected $fstabType = FstabEntryList::class;
  protected $fstabDataType = '';
  protected $hostsType = HostsEntryList::class;
  protected $hostsDataType = '';
  /**
   * @var string
   */
  public $issue;
  protected $nfsExportsType = NfsExportList::class;
  protected $nfsExportsDataType = '';
  /**
   * @var string
   */
  public $selinuxMode;

  /**
   * @param FstabEntryList
   */
  public function setFstab(FstabEntryList $fstab)
  {
    $this->fstab = $fstab;
  }
  /**
   * @return FstabEntryList
   */
  public function getFstab()
  {
    return $this->fstab;
  }
  /**
   * @param HostsEntryList
   */
  public function setHosts(HostsEntryList $hosts)
  {
    $this->hosts = $hosts;
  }
  /**
   * @return HostsEntryList
   */
  public function getHosts()
  {
    return $this->hosts;
  }
  /**
   * @param string
   */
  public function setIssue($issue)
  {
    $this->issue = $issue;
  }
  /**
   * @return string
   */
  public function getIssue()
  {
    return $this->issue;
  }
  /**
   * @param NfsExportList
   */
  public function setNfsExports(NfsExportList $nfsExports)
  {
    $this->nfsExports = $nfsExports;
  }
  /**
   * @return NfsExportList
   */
  public function getNfsExports()
  {
    return $this->nfsExports;
  }
  /**
   * @param string
   */
  public function setSelinuxMode($selinuxMode)
  {
    $this->selinuxMode = $selinuxMode;
  }
  /**
   * @return string
   */
  public function getSelinuxMode()
  {
    return $this->selinuxMode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GuestConfigDetails::class, 'Google_Service_MigrationCenterAPI_GuestConfigDetails');
