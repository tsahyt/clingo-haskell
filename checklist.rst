Checklist to keep track of which parts of the Raw API are covered by the high-level API

Raw.Asynchronous
================
+ [X] 2016-10-25 solveAsyncCancel :: MonadIO m => AsyncSolver -> m CBool
+ [o] solveAsyncGet :: MonadIO m => AsyncSolver -> Ptr SolveResult -> m CBool
+ [X] 2016-10-25 solveAsyncWait :: MonadIO m => AsyncSolver -> CDouble -> Ptr CBool 

Raw.Basic
=========
+ [X] 2016-10-25 errorCode :: MonadIO m => m ClingoError
+ [X] 2016-10-25 errorMessage :: MonadIO m => m CString
+ [ ] errorString :: MonadIO m => ClingoError -> m CString
+ [ ] setError :: MonadIO m => ClingoError -> CString -> m ()
+ [X] 2016-10-25 version :: MonadIO m => Ptr CInt -> Ptr CInt -> Ptr CInt -> m ()
+ [ ] warningString :: MonadIO m => ClingoWarning -> m CString

Raw.Configuration
=================
+ [ ] configurationArrayAt :: MonadIO m => Configuration -> Identifier -> CSize -> Ptr Identifier -> m CBool
+ [ ] configurationArraySize :: MonadIO m => Configuration -> Identifier -> Word64 -> Ptr CSize -> m CBool
+ [ ] configurationDescription :: MonadIO m => Configuration -> Identifier -> Ptr CString -> m CBool
+ [ ] configurationMapAt :: MonadIO m => Configuration -> Identifier -> CString -> Ptr Identifier -> m CBool
+ [ ] configurationMapSize :: MonadIO m => Configuration -> Identifier -> Ptr CSize -> m CBool
+ [ ] configurationMapSubkeyName :: MonadIO m => Configuration -> Identifier -> CSize -> Ptr CString -> m CBool
+ [ ] configurationRoot :: MonadIO m => Configuration -> Ptr Identifier -> m CBool
+ [ ] configurationType :: MonadIO m => Configuration -> Identifier -> Ptr ConfigurationType -> m CBool
+ [ ] configurationValueGet :: MonadIO m => Configuration -> Identifier -> CString -> CSize -> m CBool
+ [ ] configurationValueGetSize :: MonadIO m => Configuration -> Identifier -> Ptr CSize -> m CBool
+ [ ] configurationValueIsAssigned :: MonadIO m => Configuration -> Identifier -> Ptr CBool -> m CBool
+ [ ] configurationValueSet :: MonadIO m => Configuration -> Identifier -> CString -> m CBool

Raw.Control
===========
+ [ ] controlAdd :: MonadIO m => Control -> CString -> Ptr CString -> CSize 
+ [ ] controlAssignExternal :: MonadIO m => Control -> Symbol -> TruthValue 
+ [ ] controlBackend :: MonadIO m => Control -> Ptr Backend -> m CBool
+ [ ] controlClaspFacade :: MonadIO m => Control -> Ptr (Ptr ()) -> m CBool
+ [ ] controlCleanup :: MonadIO m => Control -> m CBool
+ [ ] controlConfiguration :: MonadIO m => Control -> Ptr Configuration
+ [X] 2016-10-25 controlFree :: MonadIO m => Control -> m ()
+ [ ] controlGetConst :: MonadIO m => Control -> CString -> Ptr Symbol -> m CBool
+ [o] controlGround :: MonadIO m => Control -> Ptr Part -> CSize 
+ [ ] controlHasConst :: MonadIO m => Control -> CString -> Ptr CBool -> m CBool
+ [ ] controlInterrupt :: MonadIO m => Control -> m ()
+ [X] 2016-10-25 controlLoad :: MonadIO m => Control -> CString -> m CBool
+ [X] 2016-10-25 controlNew :: MonadIO m => Ptr CString -> CSize -> FunPtr (Logger a) 
+ [ ] controlProgramBuilder :: MonadIO m => Control -> Ptr ProgramBuilder
+ [ ] controlRegisterObserver :: MonadIO m => Control 
+ [ ] controlRegisterPropagator :: MonadIO m => Control -> Ptr (Propagator a) 
+ [ ] controlReleaseExternal :: MonadIO m => Control -> Symbol -> m CBool
+ [o] controlSolve :: MonadIO m => Control -> FunPtr (CallbackModel a) -> Ptr a 
+ [ ] controlSolveAsync :: MonadIO m => Control -> FunPtr (CallbackModel a) 
+ [ ] controlSolveIter :: MonadIO m => Control -> Ptr SymbolicLiteral -> CSize 
+ [ ] controlStatistics :: MonadIO m => Control -> Ptr Statistics -> m CBool
+ [ ] controlSymbolicAtoms :: MonadIO m => Control -> Ptr SymbolicAtoms
+ [ ] controlTheoryAtoms :: MonadIO m => Control -> Ptr TheoryAtoms
+ [ ] controlUseEnumAssumption :: MonadIO m => Control -> CBool -> m CBool

Raw.Iterative
=============
+ [ ] solveIterativelyClose :: MonadIO m 
+ [ ] solveIterativelyGet :: MonadIO m 
+ [ ] solveIterativelyNext :: MonadIO m 

Raw.Model
=========
+ [ ] modelContains :: MonadIO m => Model -> Symbol -> Ptr CBool -> m CBool
+ [ ] modelContext :: MonadIO m => Model -> Ptr SolveControl -> m CBool
+ [ ] modelCost :: MonadIO m => Model -> Ptr Int64 -> CSize -> m CBool
+ [ ] modelCostSize :: MonadIO m => Model -> Ptr CSize -> m CBool
+ [ ] modelNumber :: MonadIO m => Model -> Ptr Word64 -> m CBool
+ [ ] modelOptimalityProven :: MonadIO m => Model -> Ptr CBool -> m CBool
+ [ ] modelSymbols :: MonadIO m => Model -> ShowFlag -> Ptr Symbol -> CSize 
+ [ ] modelSymbolsSize :: MonadIO m => Model -> ShowFlag -> Ptr CSize -> m CBool
+ [ ] modelType :: MonadIO m => Model -> ModelType -> m CBool
+ [ ] solveControlAddClause :: MonadIO m => SolveControl -> Ptr SymbolicLiteral 
+ [ ] solveControlThreadId :: MonadIO m => SolveControl -> Ptr Identifier 

Raw.ProgramBuilding
===================
+ [ ] backendAcycEdge :: MonadIO m => Backend -> CInt -> CInt -> Ptr Literal -> CSize -> m CBool
+ [ ] backendAddAtom :: MonadIO m => Backend -> Ptr Atom -> m CBool
+ [ ] backendAssume :: MonadIO m => Backend -> Ptr Literal -> CSize -> m CBool
+ [ ] backendBuilderAdd :: MonadIO m => ProgramBuilder -> Ptr AstStatement -> m CBool
+ [ ] backendBuilderBegin :: MonadIO m => ProgramBuilder -> m CBool
+ [ ] backendBuilderEnd :: MonadIO m => ProgramBuilder -> m CBool
+ [ ] backendExternal :: MonadIO m => Backend -> Atom -> ExternalType -> m CBool
+ [ ] backendHeuristic :: MonadIO m => Backend -> Atom -> HeuristicType -> CInt -> CUInt -> Ptr Literal -> CSize -> m CBool
+ [ ] backendMinimize :: MonadIO m => Backend -> Weight -> Ptr WeightedLiteral -> CSize -> m CBool
+ [ ] backendProject :: MonadIO m => Backend -> Ptr Atom -> CSize -> m CBool
+ [ ] backendRule :: MonadIO m => Backend -> CBool -> Ptr Atom -> CSize -> Ptr Literal -> CSize -> m CBool
+ [ ] backendWeightRule :: MonadIO m => Backend -> CBool -> Ptr Atom -> CSize -> Weight -> Ptr WeightedLiteral -> CSize -> m CBool

Raw.Propagation
===============
+ [ ] assignmentDecision :: MonadIO m => Assignment -> Word32 -> Ptr Literal -> m CBool
+ [ ] assignmentDecisionLevel :: MonadIO m => Assignment -> m Word32
+ [ ] assignmentHasConflict :: MonadIO m => Assignment -> m CBool
+ [ ] assignmentHasLiteral :: MonadIO m => Assignment -> Literal -> m CBool
+ [ ] assignmentIsFalse :: MonadIO m => Assignment -> Literal -> Ptr CBool -> m CBool
+ [ ] assignmentIsFixed :: MonadIO m => Assignment -> Literal -> Ptr CBool -> m CBool
+ [ ] assignmentIsTrue :: MonadIO m => Assignment -> Literal -> Ptr CBool -> m CBool
+ [ ] assignmentLevel :: MonadIO m => Assignment -> Literal -> Ptr Word32 -> m CBool
+ [ ] assignmentTruthValue :: MonadIO m => Assignment -> Literal -> Ptr TruthValue -> m CBool
+ [ ] propagateControlAddClause :: MonadIO m => PropagateControl -> Ptr Literal -> CSize -> ClauseType -> Ptr CBool -> m CBool
+ [ ] propagateControlAddLiteral :: MonadIO m => PropagateControl -> Ptr Literal -> m CBool
+ [ ] propagateControlAddWatch :: MonadIO m => PropagateControl -> Literal -> m CBool
+ [ ] propagateControlAssignment :: MonadIO m => PropagateControl -> m Assignment
+ [ ] propagateControlHasWatch :: MonadIO m => PropagateControl -> Literal -> m CBool
+ [ ] propagateControlPropagate :: MonadIO m => PropagateControl -> Ptr CBool -> m CBool
+ [ ] propagateControlRemoveWatch :: MonadIO m => PropagateControl -> Literal -> m ()
+ [ ] propagateControlThreadId :: MonadIO m => PropagateControl -> m Identifier
+ [ ] propagateInitAddWatch :: MonadIO m => PropagateInit -> Literal -> m CBool
+ [ ] propagateInitNumberOfThreads :: MonadIO m => PropagateInit -> m CInt
+ [ ] propagateInitSolverLiteral :: MonadIO m => PropagateInit -> Literal -> Ptr Literal -> m CBool
+ [ ] propagateInitSymbolicAtoms :: MonadIO m => PropagateInit -> Ptr SymbolicAtoms -> m CBool
+ [ ] propagateInitTheoryAtoms :: MonadIO m => PropagateInit -> Ptr TheoryAtoms -> m CBool

Raw.Statistics
==============
+ [ ] statisticsArrayAt :: MonadIO m => Statistics -> Word64 -> CSize 
+ [ ] statisticsArraySize :: MonadIO m => Statistics -> Word64 -> Ptr Word64 
+ [ ] statisticsMapAt :: MonadIO m => Statistics -> Word64 -> Ptr CString 
+ [ ] statisticsMapSize :: MonadIO m => Statistics -> Word64 -> Ptr CSize 
+ [ ] statisticsMapSubkeyName :: MonadIO m => Statistics -> Word64 -> CSize 
+ [ ] statisticsRoot :: MonadIO m => Statistics -> Ptr Word64 -> m CBool
+ [ ] statisticsType :: MonadIO m => Statistics -> Word64 -> Ptr StatisticsType 
+ [ ] statisticsValueGet :: MonadIO m => Statistics -> Word64 -> Ptr CString 

Raw.Symbol
==========
+ [o] addString :: MonadIO m => CString -> Ptr CString -> m CBool
+ [ ] parseTerm :: MonadIO m => CString -> FunPtr (Logger a) -> Ptr a -> CUInt 
+ [ ] signatureArity :: Signature -> Word32
+ [ ] signatureCreate :: MonadIO m => CString -> Word32 -> CBool -> Ptr Signature 
+ [ ] signatureHash :: Signature -> CSize
+ [ ] signatureIsEqualTo :: Signature -> Signature -> CBool
+ [ ] signatureIsLessThan :: Signature -> Signature -> CBool
+ [ ] signatureIsNegative :: Signature -> CBool
+ [ ] signatureIsPositive :: Signature -> CBool
+ [ ] signatureName :: Signature -> CString
+ [ ] symbolArguments :: MonadIO m => Symbol -> Ptr (Ptr Symbol) -> Ptr CSize 
+ [ ] symbolCreateFunction :: MonadIO m => CString -> Ptr Symbol -> CSize -> CBool 
+ [ ] symbolCreateId :: MonadIO m => CString -> CBool -> Ptr Symbol -> m CBool
+ [X] 2016-10-25 symbolCreateInfimum :: MonadIO m => Ptr Symbol -> m ()
+ [X] 2016-10-25 symbolCreateNumber :: MonadIO m => CInt -> Ptr Symbol -> m ()
+ [ ] symbolCreateString :: MonadIO m => CString -> Ptr Symbol -> m CBool
+ [X] 2016-10-25 symbolCreateSupremum :: MonadIO m => Ptr Symbol -> m ()
+ [X] 2016-10-25 symbolHash :: Symbol -> CSize
+ [X] 2016-10-25 symbolIsEqualTo :: Symbol -> Symbol -> CBool
+ [X] 2016-10-25 symbolIsLessThan :: Symbol -> Symbol -> CBool
+ [X] 2016-10-25 symbolIsNegative :: MonadIO m => Symbol -> Ptr CBool -> m CBool
+ [X] 2016-10-25 symbolIsPositive :: MonadIO m => Symbol -> Ptr CBool -> m CBool
+ [ ] symbolName :: MonadIO m => Symbol -> Ptr CString -> m CBool
+ [ ] symbolNumber :: MonadIO m => Symbol -> Ptr CInt -> m CBool
+ [ ] symbolString :: MonadIO m => Symbol -> Ptr CString -> m CBool
+ [ ] symbolSymbolToString :: MonadIO m => Symbol -> Ptr CChar -> m CBool
+ [ ] symbolSymbolToStringSize :: MonadIO m => Symbol -> Ptr CSize -> m CBool
+ [X] 2016-10-25 symbolType :: Symbol -> SymbolType
