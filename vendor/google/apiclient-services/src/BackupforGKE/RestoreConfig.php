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

namespace Google\Service\BackupforGKE;

class RestoreConfig extends \Google\Collection
{
  protected $collection_key = 'volumeDataRestorePolicyBindings';
  /**
   * @var bool
   */
  public $allNamespaces;
  /**
   * @var string
   */
  public $clusterResourceConflictPolicy;
  protected $clusterResourceRestoreScopeType = ClusterResourceRestoreScope::class;
  protected $clusterResourceRestoreScopeDataType = '';
  protected $excludedNamespacesType = Namespaces::class;
  protected $excludedNamespacesDataType = '';
  /**
   * @var string
   */
  public $namespacedResourceRestoreMode;
  /**
   * @var bool
   */
  public $noNamespaces;
  protected $restoreOrderType = RestoreOrder::class;
  protected $restoreOrderDataType = '';
  protected $selectedApplicationsType = NamespacedNames::class;
  protected $selectedApplicationsDataType = '';
  protected $selectedNamespacesType = Namespaces::class;
  protected $selectedNamespacesDataType = '';
  protected $substitutionRulesType = SubstitutionRule::class;
  protected $substitutionRulesDataType = 'array';
  protected $transformationRulesType = TransformationRule::class;
  protected $transformationRulesDataType = 'array';
  /**
   * @var string
   */
  public $volumeDataRestorePolicy;
  protected $volumeDataRestorePolicyBindingsType = VolumeDataRestorePolicyBinding::class;
  protected $volumeDataRestorePolicyBindingsDataType = 'array';

  /**
   * @param bool
   */
  public function setAllNamespaces($allNamespaces)
  {
    $this->allNamespaces = $allNamespaces;
  }
  /**
   * @return bool
   */
  public function getAllNamespaces()
  {
    return $this->allNamespaces;
  }
  /**
   * @param string
   */
  public function setClusterResourceConflictPolicy($clusterResourceConflictPolicy)
  {
    $this->clusterResourceConflictPolicy = $clusterResourceConflictPolicy;
  }
  /**
   * @return string
   */
  public function getClusterResourceConflictPolicy()
  {
    return $this->clusterResourceConflictPolicy;
  }
  /**
   * @param ClusterResourceRestoreScope
   */
  public function setClusterResourceRestoreScope(ClusterResourceRestoreScope $clusterResourceRestoreScope)
  {
    $this->clusterResourceRestoreScope = $clusterResourceRestoreScope;
  }
  /**
   * @return ClusterResourceRestoreScope
   */
  public function getClusterResourceRestoreScope()
  {
    return $this->clusterResourceRestoreScope;
  }
  /**
   * @param Namespaces
   */
  public function setExcludedNamespaces(Namespaces $excludedNamespaces)
  {
    $this->excludedNamespaces = $excludedNamespaces;
  }
  /**
   * @return Namespaces
   */
  public function getExcludedNamespaces()
  {
    return $this->excludedNamespaces;
  }
  /**
   * @param string
   */
  public function setNamespacedResourceRestoreMode($namespacedResourceRestoreMode)
  {
    $this->namespacedResourceRestoreMode = $namespacedResourceRestoreMode;
  }
  /**
   * @return string
   */
  public function getNamespacedResourceRestoreMode()
  {
    return $this->namespacedResourceRestoreMode;
  }
  /**
   * @param bool
   */
  public function setNoNamespaces($noNamespaces)
  {
    $this->noNamespaces = $noNamespaces;
  }
  /**
   * @return bool
   */
  public function getNoNamespaces()
  {
    return $this->noNamespaces;
  }
  /**
   * @param RestoreOrder
   */
  public function setRestoreOrder(RestoreOrder $restoreOrder)
  {
    $this->restoreOrder = $restoreOrder;
  }
  /**
   * @return RestoreOrder
   */
  public function getRestoreOrder()
  {
    return $this->restoreOrder;
  }
  /**
   * @param NamespacedNames
   */
  public function setSelectedApplications(NamespacedNames $selectedApplications)
  {
    $this->selectedApplications = $selectedApplications;
  }
  /**
   * @return NamespacedNames
   */
  public function getSelectedApplications()
  {
    return $this->selectedApplications;
  }
  /**
   * @param Namespaces
   */
  public function setSelectedNamespaces(Namespaces $selectedNamespaces)
  {
    $this->selectedNamespaces = $selectedNamespaces;
  }
  /**
   * @return Namespaces
   */
  public function getSelectedNamespaces()
  {
    return $this->selectedNamespaces;
  }
  /**
   * @param SubstitutionRule[]
   */
  public function setSubstitutionRules($substitutionRules)
  {
    $this->substitutionRules = $substitutionRules;
  }
  /**
   * @return SubstitutionRule[]
   */
  public function getSubstitutionRules()
  {
    return $this->substitutionRules;
  }
  /**
   * @param TransformationRule[]
   */
  public function setTransformationRules($transformationRules)
  {
    $this->transformationRules = $transformationRules;
  }
  /**
   * @return TransformationRule[]
   */
  public function getTransformationRules()
  {
    return $this->transformationRules;
  }
  /**
   * @param string
   */
  public function setVolumeDataRestorePolicy($volumeDataRestorePolicy)
  {
    $this->volumeDataRestorePolicy = $volumeDataRestorePolicy;
  }
  /**
   * @return string
   */
  public function getVolumeDataRestorePolicy()
  {
    return $this->volumeDataRestorePolicy;
  }
  /**
   * @param VolumeDataRestorePolicyBinding[]
   */
  public function setVolumeDataRestorePolicyBindings($volumeDataRestorePolicyBindings)
  {
    $this->volumeDataRestorePolicyBindings = $volumeDataRestorePolicyBindings;
  }
  /**
   * @return VolumeDataRestorePolicyBinding[]
   */
  public function getVolumeDataRestorePolicyBindings()
  {
    return $this->volumeDataRestorePolicyBindings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RestoreConfig::class, 'Google_Service_BackupforGKE_RestoreConfig');
